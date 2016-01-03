#Sample Data
library(stringi)
calcNumberOfLines <- function (fileName,seed) {
        set.seed(seed)
        cmd <- sprintf("wc -l %s",fileName)
        val <- system(cmd,intern = TRUE)
        val <- stri_trim_both(val)
        lineCount  <- as.integer(regmatches(val,regexpr("^[0-9]+",val)))
}
##sampleTestData function provides the ability to 
sampleDataLineIndexes <- function(fileName ,seed=2096) {
        lineCount <- calcNumberOfLines(fileName,seed)

        prob <- 1/((log10(lineCount))*5)
        testCases <- rbinom(lineCount,3,prob)
        testCases
}
splitDataLineIndexes <- function(fileName,nSets=10,seed=2096){
        lineCount <- calcNumberOfLines(fileName,seed=seed)
        vector <- round(runif(lineCount,min = 1,max=nSets),0) 
        return(vector)
}
splitFileNames <- function(fileName,count) {
       
                fileExt <-  regmatches(fileName,regexpr("\\.[a-z0-9]{1,3}$",fileName))
                filePrefix  <-  regmatches(fileName,regexpr("\\.[a-z0-9]{1,3}$",fileName),invert = TRUE)    
                filePrefix  <- filePrefix[[1]][1]
                indexes <- sprintf("%04.0f",1:count)
                paste(filePrefix,"_",indexes,fileExt,sep="")
      
}
trainingFileNames <- function (fileName) {
        fileExt <-  regmatches(fileName,regexpr("\\.[a-z0-9]{1,3}$",fileName))
        filePrefix  <-  regmatches(fileName,regexpr("\\.[a-z0-9]{1,3}$",fileName),invert = TRUE)    
        filePrefix  <- filePrefix[[1]][1]
        paste(filePrefix,c(".train",".valid",".test",".testmore"),fileExt,sep="")
}

writeSplitCorpusDataSets <- function(fileName, maxLineCount = 100000) {
        #Divide the file into enough files so that each file has about 100,000 lines
        #This needs to be randomly sampled.
        lineCount <- calcNumberOfLines(fileName,seed=101)
        fileCount  <- max(lineCount/maxLineCount,1) #Make sure there is at least one file
        indexes <- splitDataLineIndexes(fileName,nSets = fileCount)
        fileNames <- splitFileNames(fileName,fileCount)
        #Zero out each file
        for(i in 1:fileCount){
                fName <- fileNames[1]
                trainF <- file(fName,open = "w" )   
                close(trainF)
        }
        fileConnections <- lapply(fileNames, function(fName) {
                file(fName,open = "a" )   
        })
        
        
        input <- file(fileName,open = "r")
        for(i in indexes) {
        line <- readLines(input,1)
        conn <- fileConnections[[i]]
        writeLines(line,conn)
        } 
        close(input)
        lapply(fileConnections, close)
}
#writeSplitCorpusDataSets("./data/en_US.twitter.testmore.txt")
#writeSplitCorpusDataSets("./data/en_US.news.testmore.txt")

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
