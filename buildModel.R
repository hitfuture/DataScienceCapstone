library(tm)
library(dplyr)
library(RWeka)
library(slam)
library(testthat)
library(RJSONIO)
library(tm)
Sys.setenv(NOAWT = TRUE)

curseStops  <- names(fromJSON("./data/google_twunter_lol.json"))
curseCorp <- VCorpus(VectorSource(curseStops))
delims <- " \\t\\r\\n.!?,;\"()"


buildTermMap <- function(fileName,keepApostrophe = TRUE) {
        runModel({
                #   fconn <-  file("./data/en_US.blogs.train.txt")
                fconn <-  file(fileName)
                
                sourceData <- readLines(fconn)#,100000)
                close(fconn)
                
                dataSource <- VectorSource(x = sourceData)
                corpus <- VCorpus(dataSource)
        },"Read  data source, and create corpus")
        
        removeEmoticons <- function(x) {
                iconv(
                        x = x,from = "latin1",to = "ASCII",mark = TRUE,sub = ""
                )
        }
        
        skipWords <- function(x)
                removeWords(x, curseStops)
        as.lower <- function(x)
                content_transformer(tolower)
        removePunctuationAndNumbers <- function(x) {
                gsub("[^[:alnum:][:space:]']","", x)
        }
        removeHashTags <- function(x) {
                gsub("#\\S+","", x)
        }
        
        list.of.functions <-  if (keepApostrophe) {
                list(
                        content_transformer(removeEmoticons),
                        stripWhitespace,
                        skipWords,
                        content_transformer(removeHashTags),
                        content_transformer(removePunctuationAndNumbers),
                        removeNumbers,
                        content_transformer(tolower)
                        
                )
        } else {
                list(
                        content_transformer(removeEmoticons),
                        stripWhitespace,
                        skipWords,
                        content_transformer(removeHashTags),
                        removePunctuation,
                        removeNumbers ,
                        content_transformer(tolower)
                        
                )
        }
        
        runModel({
                bcMap <-
                        tm_map(
                                corpus, FUN = tm_reduce, tmFuns = list.of.functions,lazy = TRUE
                        )
        },"process  corpus - remove curse words, and transform to lower")
        return(bcMap)
}

#Information on how to optimize matrix build using slam
#http://stackoverflow.com/questions/21921422/row-sum-for-large-term-document-matrix-simple-triplet-matrix-tm-package

buildUnigramMap <- function(termMap) {
        UnigramTokenizer <-
                function(x)
                        RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1,delimiters =delims))
        message("building term document matrix")
        tdmap <-
                TermDocumentMatrix(termMap, control = list(tokenize = UnigramTokenizer, wordLengths =
                                                                   c(1,Inf)))
        message("rolling up")
        tdmap <- rollup(tdmap,2,na.rm = TRUE,FUN = sum)
        message("rollup complete")
        return(tdmap)
        # row_sums(tdmap,na.rm=TRUE)
        
}

buildBigramMap <- function(termMap) {
        BigramTokenizer <-
                function(x)
                        RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2,delimiters =delims))
        tdmap <-
                TermDocumentMatrix(termMap, control = list(tokenize = BigramTokenizer))
        
        message("rolling up")
        tdmap <- rollup(tdmap,2,na.rm = TRUE,FUN = sum)
        message("rollup complete")
        return(tdmap)
}

buildTrigramMap <- function(termMap) {
        TrigramTokenizer <-
                function(x)
                        RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3,delimiters =delims))
        tdmap <-
                TermDocumentMatrix(termMap, control = list(tokenize = TrigramTokenizer))
        message("rolling up")
        tdmap <- rollup(tdmap,2,na.rm = TRUE,FUN = sum)
        message("rollup complete")
        return(tdmap)
        
}

buildQuadgramMap <- function(termMap) {
        QuadgramTokenizer <-
                function(x)
                        RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 4, max = 4,delimiters =delims))
        tdmap <-
                TermDocumentMatrix(termMap, control = list(tokenize = QuadgramTokenizer))
        message("rolling up")
        tdmap <- rollup(tdmap,2,na.rm = TRUE,FUN = sum)
        message("rollup complete")
        return(tdmap)
}

buildFrequencyDataSet <- function(tdmap) {
        m <- as.matrix((tdmap))
        v <- sort(rowSums(m),decreasing = TRUE)
        d <- data.frame(word = (names(v)),freq = v)
        grams <-
                sapply(as.character(d$word), function(x) {
                        rev(strsplit(x,split = " ",fixed = TRUE))
                })
        wordCount  <- length(grams[[1]])
        
        columnNames <- paste("w_",(wordCount:1) - 1,sep = "")
        for (i in 1:wordCount) {
                vector <- sapply(grams , function(x)
                        x[i])
                d[,(columnNames[i])] <- vector
                
        }
        
        d
}

#Assign Probability
freqOfWord <- function(wrds , unigram) {
        unigram[wrds,]$freq
}

#Compute the probability of the second to the last word in an Ngram
computeProbability <- function (ngram, unigram) {
        uni <-  freqOfWord((ngram$w_1),unigram)
        ngram$freq.w_1 <- uni
        ngram %>% mutate(prob = freq / freq.w_1)
}


assignProbability <- function(data, uni) {
        #         lapply(data$)
        #         data <- data%>%mutate(prob =  )
}