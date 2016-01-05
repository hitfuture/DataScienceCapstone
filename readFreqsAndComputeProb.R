library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)



rm(list=ls())

source("goodTuring.R")
message("read uni")
freq.uni <- fread("./data/freq/freq_unigram.opt.n.csv",header = TRUE,verbose = TRUE,showProgress = TRUE)
setkey(freq.uni,word)
message("read bi")
freq.bi <- fread("./data/freq/freq_bigram.opt.n.csv",header = TRUE,verbose = TRUE,showProgress = TRUE)
setkey(freq.bi,word)
message("read tri")
freq.tri <- fread("./data/freq/freq_trigram.opt.n.csv",header = TRUE,verbose = TRUE,showProgress = TRUE)
setkey(freq.tri,word)

message(("read quad"))
freq.quad <- fread("./data/freq/freq_quadgram.opt.n.csv",header = TRUE,verbose = TRUE,showProgress = TRUE)
setkey(freq.quad,word)

a1 <- freq.quad%>%filter(words=="live and id")

nfreq.uni <- calc.nfreq(freq.uni,type="uni")
nfreq.bi  <- calc.nfreq(freq.bi,type="bi")
setkey(nfreq.bi,freq)
nfreq.tri <- calc.nfreq(freq.tri,type="tri")
nfreq.qud <- calc.nfreq(freq.quad,type="quad")
nfreq <- rbind(nfreq.uni,nfreq.bi,nfreq.tri,nfreq.qud)



p <- ggplot(nfreq,aes(x=log10(freq),y=log10(n_r),color=type))+
        geom_point()+ 
        theme_bw()+

        facet_grid(type~.,as.table = TRUE)
p
nfreq.bi
n1 <- nfreq.bi[1,n_r]
n2 <- nfreq.bi[2,n_r]
(2*n2)/n1
d.bi <-calc.nrz.estimate(nfreq.bi)
d.uni <- calc.nrz.estimate(nfreq.uni)
d.q<- calc.nrz.estimate(nfreq.qud)

p <- ggplot(nfreq.bi,aes(x=log10(freq),y=log10(d.bi),color=type))+
        geom_point()+ 
        theme_bw()+
        facet_grid(type~.,as.table = TRUE)
p


p <- ggplot(nfreq.qud,aes(x=log10(freq),y=log10(d.q),color=type))+
        geom_point()+ 
        theme_bw()+
        facet_grid(type~.,as.table = TRUE)
p

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