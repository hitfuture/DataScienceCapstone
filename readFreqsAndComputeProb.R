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
nfreq.tri <- calc.nfreq(freq.tri,type="tri")
nfreq.quad <- calc.nfreq(freq.quad,type="quad")
nfreq <- rbind(nfreq.uni,nfreq.bi,nfreq.tri,nfreq.quad)
nfreq$type <- factor(nfreq$type,levels=c("uni","bi","tri","quad"),
                     labels = c("unigram","bigram","trigram","quadgram"),
                     ordered = TRUE)


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


p <- ggplot(nfreq.quad,aes(x=log10(freq),y=log10(gt),color=type))+
        geom_point()+ 
        theme_bw()+
        facet_grid(type~.,as.table = TRUE)
p

#gtanal.S

#read in data 
#xm<-matrix(scan("freqhist",0),ncol=2,byrow=T)
#frequencyTable <- read.csv("freqTable.csv")

# 
# #output 
# cat(xN,sum(xnr),"0",file="gtanal") 
# cat(0,xnr[1]/xN,"0",file="gtanal",append=TRUE) 
# for(i in 1:length(xr)) cat(xr[i],xr[i]*xrstcmbrel[i],"0", 
#                           file="gtanal", append=TRUE)


nfreq.uni$gt <- computeGoodTuring(nfreq.uni,FALSE)
freq.uni<-left_join(freq.uni,nfreq.uni,c("freq"="freq"))
nfreq.bi$gt <- computeGoodTuring(nfreq.bi,FALSE)
freq.bi<-left_join(freq.bi,nfreq.bi,c("freq"="freq"))

nfreq.tri$gt <- computeGoodTuring(nfreq.tri,FALSE)
freq.tri<-left_join(freq.tri,nfreq.tri,c("freq"="freq"))

nfreq.quad$gt <- computeGoodTuring(nfreq.quad,FALSE)
freq.quad<-left_join(freq.quad,nfreq.quad,c("freq"="freq"))

nfreq <- rbind(nfreq.uni,nfreq.bi,nfreq.tri,nfreq.quad)

nfreq$type <- factor(nfreq$type,levels=c("uni","bi","tri","quad"),
                     labels = c("unigram","bigram","trigram","quadgram"),
                     ordered = TRUE)
p <- ggplot(nfreq,aes(x=log10(freq),y=log10(gt),color=type))+
        geom_point()+ 
        theme_bw()+
        facet_grid(type~.,as.table = TRUE)
print(p)

isSimple <- TRUE
nfreq.uni$gt <- computeGoodTuring(nfreq.uni,isSimple)
freq.uni.2<-left_join(freq.uni,nfreq.uni,c("freq"="freq"))
nfreq.bi$gt <- computeGoodTuring(nfreq.bi,isSimple)
freq.bi.2<-left_join(freq.bi,nfreq.bi,c("freq"="freq"))

nfreq.tri$gt <- computeGoodTuring(nfreq.tri,isSimple)
freq.tri.2<-left_join(freq.tri,nfreq.tri,c("freq"="freq"))

nfreq.quad$gt <- computeGoodTuring(nfreq.quad,isSimple)
freq.quad.2<-left_join(freq.quad,nfreq.quad,c("freq"="freq"))

nfreq <- rbind(freq.uni,freq.bi,freq.tri,freq.quad)

nfreq$type <- factor(nfreq$type,levels=c("uni","bi","tri","quad"),
                     labels = c("unigram","bigram","trigram","quadgram"),
                     ordered = TRUE)
p <- ggplot(nfreq,aes(x=log10(freq),y=log10(gt),color=type))+
        geom_point()+ 
        theme_bw()+
        facet_grid(type~.,as.table = TRUE)
print(p)