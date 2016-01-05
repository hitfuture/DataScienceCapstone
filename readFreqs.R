library(dplyr)
library(tidyr)
library(data.table)
rcnt <- -1
message("reading uni")

freq <- fread("./data/freq_unigram.csv",drop = c(1),nrows = rcnt)

#setkey(freq,word)
str(freq)
write.csv(freq,"./data/freq/freq_unigram.opt.csv",row.names=FALSE)
#Next write it so that it has the overal frequency


message("reading bi")

freq.bi <- fread("./data/freq_bigram.csv",drop = c(1),nrows = rcnt)
freq.bi <- freq.bi%>%unite(words,w_1,sep=" ")
#setkey(freq.bi,word)


str(freq.bi)
write.csv(freq.bi,"./data/freq/freq_bigram.opt.csv",row.names=FALSE,row.names=FALSE)

message("reading tri")

freq.tri <- fread("./data/freq_trigram.csv",drop = c(1),nrows = rcnt)
freq.tri <- freq.tri%>%unite(words,w_2,w_1,sep=" ")

#setkey(freq.tri,word)
str(freq.tri)
write.csv(freq.tri,"./data/freq/freq_trigram.opt.csv",row.names=FALSE,row.names=FALSE)


message("reading quad")
freq.quad <- fread("./data/freq_quadgram.csv",drop = c(1),nrows = rcnt,row.names=FALSE)
freq.quad <- freq.quad%>%unite(words,w_3,w_2,w_1,sep=" ")
#setkey(freq.quad,word)

str(freq.quad)
write.csv(freq.quad,"./data/freq/freq_quadgram.opt.csv",row.names=FALSE,row.names=FALSE)
