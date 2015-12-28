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
                removeNumbers,
                content_transformer(tolower)
        )
        
        runModel({
                bcMap <- tm_map(corpus, FUN = tm_reduce, tmFuns = list.of.functions)
        },"process  corpus - remove curse words, and transform to lower")
        return(bcMap)
}



buildUnigramMap <- function(termMap) {
        TrigramTokenizer <-
                function(x)
                        RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))
        tdmap <-
                TermDocumentMatrix(termMap, control = list(tokenize = TrigramTokenizer))
        tdmap
}

buildBigramMap <- function(termMap) {
        TrigramTokenizer <-
                function(x)
                        RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))
        tdmap <-
                TermDocumentMatrix(termMap, control = list(tokenize = TrigramTokenizer))
        tdmap
}

buildTrigramMap <- function(termMap) {
        TrigramTokenizer <-
                function(x)
                        RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))
        tdmap <-
                TermDocumentMatrix(termMap, control = list(tokenize = TrigramTokenizer))
        tdmap
}


buildFrequencyDataSet <- function(tdmap) {
        m <- as.matrix((tdmap))
        v <- sort(rowSums(m),decreasing = TRUE)
        d <- data.frame(word = names(v),freq = v)
        d
}