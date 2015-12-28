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
