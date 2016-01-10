#Combine testmore with inital frequencies


library(data.table)
library(dplyr)
library(tidyr)
# uniblog1 <- fread("./data/ngramdata/en_US.blogs.testmore_0001_uni_freq.txt",stringsAsFactors = FALSE,drop=c(1,4),data.table = TRUE)
# setkey(uniblog1,word)
# uniblog1 <- fread("./data/ngramdata/en_US.blogs.testmore_0001_uni_freq.txt",stringsAsFactors = FALSE,drop=c(1,4),data.table = TRUE)
# setkey(uniblog1,word)
sourceDir <- "./data/freq/"
allfiles <- dir(sourceDir)
bifiles  <- allfiles[grep("bi",allfiles)]
nlines <- -1


#unifiles  <- allfiles[grep("uni",allfiles)]
ngramdata <-fread(paste(sourceDir,"en_US.uni.testmore.csv",sep=""),stringsAsFactors = FALSE,data.table = TRUE,nrows = nlines)
nextFile <- fread(paste(sourceDir,"freq_unigram.csv",sep=""),stringsAsFactors = FALSE,drop=c(1),data.table = TRUE,nrows = nlines)
nextFile  <- nextFile%>%select(-w_0)
ngramdata <- bind_rows(nextFile,ngramdata)
ngramdata <- ngramdata%>%group_by(word)%>%summarise_each(funs(sum))
nfreq <- ngramdata%>%count(freq,sort = TRUE)%>%rename(n_r=n)
ngramdata <- left_join(ngramdata,nfreq,by = c("freq"="freq"))
write.csv(ngramdata,"./data/en_US.uni.csv",row.names = FALSE)

f1   <- "en_US.bi.testmore.csv"
ngramdata <-fread(paste(sourceDir,f1,sep=""),stringsAsFactors = FALSE,data.table = TRUE,nrows = nlines)
f2 <- "freq_bigram.csv"
nextFile <- fread(paste(sourceDir,f2,sep=""),stringsAsFactors = FALSE,drop=c(1,7),data.table = TRUE,nrows = nlines)
nextFile  <- nextFile%>%unite(words,w_1,sep=" ")
ngramdata <- bind_rows(nextFile,ngramdata)
ngramdata <- ngramdata%>%group_by(word,words,w_0)%>%summarise_each(funs(sum))
nfreq <- ngramdata%>%count(freq,sort = TRUE)%>%rename(n_r=n)
ngramdata <- left_join(ngramdata,nfreq,by = c("freq"="freq"))
write.csv(ngramdata,"./data/en_US.bi.csv",row.names = FALSE)
 

#Trigram

f1   <- "en_US.tri.testmore.csv"
ngramdata <-fread(paste(sourceDir,f1,sep=""),stringsAsFactors = FALSE,data.table = TRUE,nrows = nlines)
f2 <- "freq_trigram.csv"
nextFile <- fread(paste(sourceDir,f2,sep=""),stringsAsFactors = FALSE,drop=c(1,8),data.table = TRUE,nrows = nlines)
nextFile <- nextFile%>%unite(words,w_2,w_1,sep=" ")
ngramdata <- bind_rows(nextFile,ngramdata)
ngramdata <- ngramdata%>%group_by(word,words,w_0)%>%summarise_each(funs(sum))
nfreq <- ngramdata%>%count(freq,sort = TRUE)%>%rename(n_r=n)
ngramdata <- left_join(ngramdata,nfreq,by = c("freq"="freq"))
write.csv(ngramdata,"./data/en_US.tri.csv",row.names = FALSE)

#Quadgram


f1   <- "en_US.quad.testmore.csv"
ngramdata <-fread(paste(sourceDir,f1,sep=""),stringsAsFactors = FALSE,data.table = TRUE,nrows = nlines)
f2 <- "freq_quadgram.csv"
nextFile <- fread(paste(sourceDir,f2,sep=""),stringsAsFactors = FALSE,drop=c(1,9),data.table = TRUE,nrows = nlines)
nextFile <- nextFile%>%unite(words,w_3,w_2,w_1,sep=" ")
ngramdata <- bind_rows(nextFile,ngramdata)
ngramdata <- ngramdata%>%group_by(word,words,w_0)%>%summarise_each(funs(sum))
nfreq <- ngramdata%>%count(freq,sort = TRUE)%>%rename(n_r=n)
ngramdata <- left_join(ngramdata,nfreq,by = c("freq"="freq"))
write.csv(ngramdata,"./data/en_US.quad.csv",row.names = FALSE) 