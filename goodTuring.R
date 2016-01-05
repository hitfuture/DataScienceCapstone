
#Perform a Simple Good-Turing analysis, using S. 
# 
#Input is a file with two items per line, frequency, r, and 
#non-zero frequencies of frequencies Nr. 
# 
#Output to stdout has a first line containing the total number of objects, N,
#and the total number of types seen. 
#The second line contains a zero and the TOTAL probability of unseen objects. 
#The following lines contain the frequency r, and the SGT r*. 
# #To get the probability for an object seen r>=1 times, divide r* by N. 
# 
#The script destroys files named "freqhist" and "gtanal" 
#and overwrites a number of S data sets, leaving the analysis to 
#be examined in S. 
# 
#Usage: gt.s inputfile 
# 
# cp $1 freqhist 
# S <gtanal.S 
# cat gtanal 
# rm freqhist gtanal


#gtfuncs.S

nrzest<-function(r, nr) { 
  d <- c(1, diff(r)) 
  dr <- c(0.5 * (d[-1] + d[ - length(d)]), d[length(d)]) 
  return(nr/dr) 
} 

rstest<-function(r, coef) { 
  num <- (r * (1 + 1/r))
  ex <- (1 + coef[2]) 
  num^ex
  
}

#gtanal.S

#read in data 
#xm<-matrix(scan("freqhist",0),ncol=2,byrow=T)
#frequencyTable <- read.csv("freqTable.csv")
xr<-frequencyTable[,1] 
xnr<-frequencyTable[,2]
xN<-sum(xr*xnr)

#make averaging transform 
xnrz<-nrzest(xr,xnr)
#get Linear Good-Turing estimate
xf<-lsfit(log(xr),log(xnrz))
xcoef<-xf$coef 
xrst<-rstest(xr,xcoef) 
xrstrel<-xrst/xr
#get Turing estimate 
xrtry<-xr==c(xr[-1]-1,0) 
xrstarel<-rep(0,length(xr)) 
xrstarel[xrtry]<-(xr[xrtry]+1)/xr[xrtry]*c(xnr[-1],0)[xrtry]/xnr[xrtry]
#make switch from Turing to LGT estimates 
tursd<-rep(1,length(xr)) 
for(i in 1:length(xr))if(xrtry[i]) 
  tursd[i]<-(i+1)/xnr[i]*sqrt(xnr[i+1]*(1+xnr[i+1]/xnr[i])) 
xrstcmbrel<-rep(0,length(xr)) 
useturing<-TRUE 
for(r in 1:length(xr)){ 
  if(!useturing) xrstcmbrel[r]<-xrstrel[r] 
  else if(abs(xrstrel-xrstarel)[r]*r/tursd[r] > 1.65) 
    
xrstcmbrel[r]<-xrstarel[r] else {useturing<-FALSE; xrstcmbrel[r]<-xrstrel[r]} }

#renormalize the probabilities for observed objects 
sumpraw<-sum(xrstcmbrel*xr*xnr/xN) 
xrstcmbrel<-xrstcmbrel*(1-xnr[1]/xN)/sumpraw

#output 
cat(xN,sum(xnr),"0",file="gtanal") 
cat(0,xnr[1]/xN,"0",file="gtanal",append=TRUE) 
for(i in 1:length(xr)) cat(xr[i],xr[i]*xrstcmbrel[i],"0", 
  file="gtanal", append=TRUE)

calc.nfreq <- function(data,type="unk") {
        nfreq <- data%>%count(freq)%>%rename(n_r=n)
        nfreq$type <-type
        setkey(nfreq,freq)
        nfreq
}
calc.nrz.estimate <- function(data){
        r <- data[,freq]
        nr <- data[,n_r]
        d <- c(1,diff(r))
        dr <- c(0.5*(d[-1] + d[- length(d)]),d[length(d)] )
        return(nr/dr)
}
