
library(data.table)
library(dplyr)
library(tidyr)
# uniblog1 <- fread("./data/ngramdata/en_US.blogs.testmore_0001_uni_freq.txt",stringsAsFactors = FALSE,drop=c(1,4),data.table = TRUE)
# setkey(uniblog1,word)
# uniblog1 <- fread("./data/ngramdata/en_US.blogs.testmore_0001_uni_freq.txt",stringsAsFactors = FALSE,drop=c(1,4),data.table = TRUE)
# setkey(uniblog1,word)
allfiles <- dir("./data/ngramdata/")
bifiles  <- allfiles[grep("bi",allfiles)]
bidata <-fread(paste("./data/ngramdata/",bifiles[1],sep=""),stringsAsFactors = FALSE,drop=c(1,7),data.table = TRUE)
bidata <- bidata%>%unite(words,w_1,sep=" ")

for(afile in bifiles[2:length(bifiles)]) {
        message(afile)
        bifileNext <- fread(paste("./data/ngramdata/",afile,sep=""),stringsAsFactors = FALSE,drop=c(1,7),data.table = TRUE)
        bifileNext  <- bifileNext%>%unite(words,w_1,sep=" ")

        bidata <- bind_rows(bifileNext,bidata)
        bidata <- bidata%>%group_by(word,words,w_0)%>%summarise_each(funs(sum))
        
}
 
write.csv(bidata,"./data/en_US.bi.testmore.csv",row.names = FALSE)
rm(list = c("bidata","bifileNext","bifiles"))

unifiles  <- allfiles[grep("uni",allfiles)]
unidata <-fread(paste("./data/ngramdata/",unifiles[1],sep=""),stringsAsFactors = FALSE,drop=c(1),data.table = TRUE)
unidata <- unidata%>%select(-w_0)

for(afile in unifiles[2:length(unifiles)]) {
        message(afile)
        unifileNext <- fread(paste("./data/ngramdata/",afile,sep=""),stringsAsFactors = FALSE,drop=c(1),data.table = TRUE)
        unifileNext  <- unifileNext%>%select(-w_0)

        
        unidata <- bind_rows(unifileNext,unidata)
}

write.csv(unidata,"./data/en_US.uni.testmore.csv",row.names = FALSE)
rm(list = c("unidata","unifileNext","unifiles"))

files  <- allfiles[grep("tri",allfiles)]
ngramdata <-fread(paste("./data/ngramdata/",files[1],sep=""),stringsAsFactors = FALSE,drop=c(1,8),data.table = TRUE)
ngramdata <- ngramdata%>%unite(words,w_2,w_1,sep=" ")

for(afile in files[2:length(files)]) {
        message(afile)
        bifileNext <- fread(paste("./data/ngramdata/",afile,sep=""),stringsAsFactors = FALSE,drop=c(1,8),data.table = TRUE)
        bifileNext  <- bifileNext%>%unite(words,w_2,w_1,sep=" ")
        
        ngramdata <- bind_rows(bifileNext,ngramdata)
        ngramdata <- ngramdata%>%group_by(word,words,w_0)%>%summarise_each(funs(sum))
        
}

write.csv(ngramdata,"./data/en_US.tri.testmore.csv",row.names = FALSE)
rm(list = c("ngramdata","bifileNext","files"))

files  <- allfiles[grep("quad",allfiles)]
ngramdata <-fread(paste("./data/ngramdata/",files[1],sep=""),stringsAsFactors = FALSE,drop=c(1,9),data.table = TRUE)
ngramdata <- ngramdata%>%unite(words,w_3,w_2,w_1,sep=" ")

for(afile in files[2:length(files)]) {
        message(afile)
        bifileNext <- fread(paste("./data/ngramdata/",afile,sep=""),stringsAsFactors = FALSE,drop=c(1,9),data.table = TRUE)
        bifileNext  <- bifileNext%>%unite(words,w_3,w_2,w_1,sep=" ")
        
        ngramdata <- bind_rows(bifileNext,ngramdata)
        ngramdata <- ngramdata%>%group_by(word,words,w_0)%>%summarise_each(funs(sum))
        
}

write.csv(ngramdata,"./data/en_US.quad.testmore.csv",row.names = FALSE)
rm(list = c("ngramdata","bifileNext","files"))
