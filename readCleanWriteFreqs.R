library(dplyr)
library(tidyr)
library(data.table)
rcnt <- -1
#rcnt <- 1000
message("reading uni")

freq <- fread("./data/freq/freq_unigram.opt.csv",nrows = rcnt)
nfreq <- freq%>%count(freq,sort=TRUE)%>%rename(n_r=n)
freq <- left_join(freq,nfreq,by=c("freq"="freq"))
#setkey(freq,word)
str(freq)
write.csv(freq,"./data/freq/freq_unigram.opt.n.csv",row.names=FALSE)
#Next write it so that it has the overal frequency


message("reading bi")

freq.bi <- fread("./data/freq/freq_bigram.opt.csv",nrows = rcnt)
#setkey(freq.bi,word)
nfreq <- freq.bi%>%count(freq,sort=TRUE)%>%rename(n_r=n)
freq.bi <- left_join(freq.bi,nfreq,by=c("freq"="freq"))

str(freq.bi)
write.csv(freq.bi,"./data/freq/freq_bigram.opt.n.csv",row.names=FALSE )

message("reading tri")

freq.tri <- fread("./data/freq/freq_trigram.opt.csv",nrows = rcnt)

#setkey(freq.tri,word)
nfreq <- freq.tri%>%count(freq,sort=TRUE)%>%rename(n_r=n)
freq.tri <- left_join(freq.tri,nfreq,by=c("freq"="freq"))
str(freq.tri)
write.csv(freq.tri,"./data/freq/freq_trigram.opt.n.csv",row.names=FALSE )


message("reading quad")
freq.quad <- fread("./data/freq/freq_quadgram.opt.csv",nrows = rcnt)
#setkey(freq.quad,word)
nfreq <- freq.quad%>%count(freq,sort=TRUE)%>%rename(n_r=n)
freq.quad <- left_join(freq.quad,nfreq,by=c("freq"="freq"))
str(freq.quad)
write.csv(freq.quad,"./data/freq/freq_quadgram.opt.n.csv",row.names=FALSE )
