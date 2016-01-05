library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)



#rm(list=ls())

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
nfreq.quad <- calc.nfreq(freq.quad,type="quad")
nfreq <- rbind(nfreq.uni,nfreq.bi,nfreq.tri,nfreq.quad)



p <- ggplot(nfreq,aes(x=log10(freq),y=log10(n_r),color=type))+
        geom_point()+ 
        theme_bw()+

        facet_grid(type~.,as.table = TRUE)
p
nfreq.bi$gt <- computeGoodTuring(nfreq.bi)



p <- ggplot(nfreq.bi,aes(x=log10(freq),y=log10(gt),color=type))+
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


#output 
cat(xN,sum(xnr),"0",file="gtanal") 
cat(0,xnr[1]/xN,"0",file="gtanal",append=TRUE) 
for(i in 1:length(xr)) cat(xr[i],xr[i]*xrstcmbrel[i],"0", 
                           file="gtanal", append=TRUE)


nfreq.uni$gt <- computeGoodTuring(nfreq.uni)
nfreq.bi$gt <- computeGoodTuring(nfreq.bi)

nfreq.tri$gt <- computeGoodTuring(nfreq.tri)
nfreq.qud$gt <- computeGoodTuring(nfreq.qud)

nfreq <- rbind(nfreq.uni,nfreq.bi,nfreq.tri,nfreq.qud)


p <- ggplot(nfreq,aes(x=log10(freq),y=log10(gt),color=type))+
        geom_point()+ 
        theme_bw()+
        facet_grid(type~.,as.table = TRUE)
p