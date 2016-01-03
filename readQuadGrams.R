
library(data.table)
library(dplyr)
library(tidyr)
allfiles <- dir("./data/ngramdata/")
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
