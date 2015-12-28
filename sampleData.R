#Sample Data
library(stringi)

##sampleTestData function provides the ability to 
sampleDataLineIndexes <- function(fileName ,seed=2096) {
        
        set.seed(seed)
        cmd <- sprintf("wc -l %s",fileName)
        val <- system(cmd,intern = TRUE)
        val <- stri_trim_both(val)
        lineCount  <- as.integer(regmatches(val,regexpr("^[0-9]+",val)))
        prob <- 1/((log10(lineCount))*5)
        testCases <- rbinom(lineCount,3,prob)
        testCases
}
trainingFileNames <- function (fileName) {
        fileExt <-  regmatches(fileName,regexpr("\\.[a-z0-9]{1,3}$",fileName))
        filePrefix  <-  regmatches(fileName,regexpr("\\.[a-z0-9]{1,3}$",fileName),invert = TRUE)    
        filePrefix  <- filePrefix[[1]][1]
        paste(filePrefix,c(".train",".valid",".test",".testmore"),fileExt,sep="")
}

writeCorpusDataSets <- function(fileName){
   input <- file(fileName,open = "r")
   trainingFileNames <- trainingFileNames(fileName)  
   
   trainFileName <- trainingFileNames[1]
   trainF <- file(trainFileName,open = "w" )   
   close(trainF)
   trainF <- file(trainFileName,open = "a" )   
   
   validFileName <- trainingFileNames[2]
   validF <- file(validFileName,open = "w")   
   close(validF)
   validF <- file(validFileName,open = "a")   
   testFileName <- trainingFileNames[3]
   testF <- file(testFileName,open = "w")   
   close(testF)
   testF <- file(testFileName,open = "a")   
   testMoreFileName <- trainingFileNames[4]
   testMoreF <- file(testMoreFileName, open = "w")
   close(testMoreF)
   testMoreF <- file(testMoreFileName, open = "a")
   
   indexes <- sampleDataLineIndexes(fileName)   
   for(n in indexes) {
           line <- readLines(input,1)
           if(n == 0) {
               writeLines(line,testMoreF)    
           } 
           if(n == 1) {
                   writeLines(line,trainF)      
           }
           if(n == 2) {
                   writeLines(line,validF)   
           }
           if(n == 3) {
                   writeLines(line,testF)   
           }
           
   }
   close(input)
   close(trainF)
   close(validF)
   close(testF)
   close(testMoreF)
}
#answer <- writeCorpusDataSets("./data/en_US.blogs.txt")

# blogVector <- sampleDataLineIndexes("./data/en_US.blogs.txt")
# table(blogVector)
# newsVector <- sampleDataLineIndexes("./data/en_US.news.txt")
# table(newsVector)
# twitterVector <- sampleDataLineIndexes("./data/en_US.twitter.txt")
# table(twitterVector)

