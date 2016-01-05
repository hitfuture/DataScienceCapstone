
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
