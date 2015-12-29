library(tm)
library(dplyr)
library(RWeka)
library(slam)
library(testthat)

buildTermMap <- function(fileName) {
        runModel({
                #   fconn <-  file("./data/en_US.blogs.train.txt")
                fconn <-  file(fileName)
                
                sourceData <- readLines(fconn)#,100000)
                close(fconn)
                
                dataSource <- VectorSource(x = sourceData)
                corpus <- VCorpus(dataSource)
        },"Read  data source, and create corpus")
        skipWords <- function(x)
                removeWords(x, curseStops)
        as.lower <- function(x)
                content_transformer(tolower)
        list.of.functions <- list(
             stripWhitespace,
            skipWords,
              removePunctuation,
               removeNumbers ,
               content_transformer(tolower) 
            
        )
          
        runModel({
                bcMap <- tm_map(corpus, FUN = tm_reduce, tmFuns = list.of.functions)
        },"process  corpus - remove curse words, and transform to lower")
        return(bcMap)
}



buildUnigramMap <- function(termMap) {
        UnigramTokenizer <-
                function(x)
                        RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))
        tdmap <-
                TermDocumentMatrix(termMap, control = list(tokenize = UnigramTokenizer, wordLengths=c(1,Inf)))
        rollup(tdmap,2,na.rm=TRUE,FUN = sum)           
       # row_sums(tdmap,na.rm=TRUE)
        
}

buildBigramMap <- function(termMap) {
        BigramTokenizer <-
                function(x)
                        RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))
        tdmap <-
                TermDocumentMatrix(termMap, control = list(tokenize = BigramTokenizer))
        
        row_sums(tdmap,na.rm=TRUE)
}

buildTrigramMap <- function(termMap) {
        TrigramTokenizer <-
                function(x)
                        RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))
        tdmap <-
                TermDocumentMatrix(termMap, control = list(tokenize = TrigramTokenizer))
        row_sums(tdmap,na.rm=TRUE)
        
}

buildQuadgramMap <- function(termMap) {
        QuadgramTokenizer <-
                function(x)
                        RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 4, max = 4))
        tdmap <-
                TermDocumentMatrix(termMap, control = list(tokenize = QuadgramTokenizer))
        row_sums(tdmap,na.rm=TRUE)
}

buildFrequencyDataSet <- function(tdmap) {
        m <- as.matrix((tdmap))
        v <- sort(rowSums(m),decreasing = TRUE)
        d <- data.frame(word = (names(v)),freq = v)
        grams <-
                sapply(as.character(d$word), function(x) { 
                        rev(strsplit(x,split = " ",fixed = TRUE))})
        wordCount  <- length(grams[[1]])
       
                columnNames <- paste("w_",(wordCount:1)-1,sep = "")
                for (i in 1:wordCount) {
                        vector <- sapply(grams , function(x)
                                x[i])
                        d[,(columnNames[i])] <- vector
                
                }
                 
        d
}

#Assign Probability
freqOfWord <- function(wrds , unigram ) {
     index <- 0
     freqs <- sapply(wrds, function(wrd) {
             index <-- index + 1
           if(index %% 100 == 0) {message(sprintf("Now at %f row",index))}  
        rec <- unigram[which(frequencyUnigramDF$word == wrd),c("freq")]
         rec}
        )
   #  expect_equal(length(wrds),length(freqs))
     freqs <- as.integer(sapply(freqs,function(x){x}))
        return((freqs))
}

#Compute the probability of the second to the last word in an Ngram
computeProbability <- function (ngram, unigram) {
        uni <-  freqOfWord((ngram$w_1),unigram)
        ngram$freq.w_1 <- uni
        ngram %>%mutate(prob = freq / freq.w_1)
}


assignProbability <- function(data, uni) {
#         lapply(data$)
#         data <- data%>%mutate(prob =  )
}