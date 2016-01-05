#Build all of the testmore models
library(RJSONIO)
library(tm)
source("sampleData.R")
source("runModel.R")
source("buildModel.R")
library(slam)
options(mc.cores=1)

freqFileNames <- function (fileNames,type) {
        fileExt <-  regmatches(fileNames,regexpr("\\.[a-z0-9]{1,3}$",fileNames))
        filePrefix  <-  regmatches(fileNames,regexpr("\\.[a-z0-9]{1,3}$",fileNames),invert = TRUE)    
        filePrefix  <- filePrefix[[1]][1]
        paste(filePrefix,"_",type,"_freq",fileExt,sep="")
}
curseStops  <- names(fromJSON("./data/google_twunter_lol.json"))
curseCorp <- VCorpus(VectorSource(curseStops))

Sys.setenv(NOAWT=TRUE)
files <- dir("./data")[grep("news.testmore_",dir("./data"))]
files <- sapply(files,function(x) paste("./data/",x,sep=""))

#sapply(files,freqFileNames,"xxx")
for(fName in files) {
         message(paste("Building Term Map for: ",fName))
        map <- buildTermMap(fName)
        message(paste("Building Term Map for: ",fName,"Complete"))
        message(paste("Building Unigram for: ",fName))
        tdMapsUnigram <- buildUnigramMap(map)
        message("Building frequency file")
         frequencyUnigramDF <- buildFrequencyDataSet(tdMapsUnigram)
         gFile <- freqFileNames(fName,"uni")
         write.csv(frequencyUnigramDF, gFile)
       
         message(paste("Building Bigram Matrix for: ",fName)) 
        tdMapsBigram <- buildBigramMap(map)
        tdMapsBigram <- removeSparseTerms(tdMapsBigram,.9999)
        message("Building frequency file")
        
        frequencyBigramDF <- buildFrequencyDataSet(tdMapsBigram)
        frequencyBigramDF <- computeProbability(frequencyBigramDF, frequencyUnigramDF)
        gFile <- freqFileNames(fName,"bi")
        write.csv(frequencyBigramDF, gFile)
        
        
        message(paste("Building Trigram Matrix for: ",fName)) 
        tdMapsTrigram <- buildTrigramMap(map)
        tdMapsTrigram <- removeSparseTerms(tdMapsTrigram,.9999)
        message("Building frequency file")
        frequencyTrigramDF <- buildFrequencyDataSet(tdMapsTrigram)
        frequencyTrigramDF <- computeProbability(frequencyTrigramDF, frequencyUnigramDF)
        gFile <- freqFileNames(fName,"tri")
        write.csv(frequencyTrigramDF, gFile)
       
         message(paste("Building Quadigram Matrix for: ",fName)) 
        
        tdMapsQuadgram <- buildQuadgramMap(map)
        tdMapsQuadgram <- removeSparseTerms(tdMapsQuadgram,.9999)
        message("Building frequency file")
        frequencyQuadgramDF <- buildFrequencyDataSet(tdMapsQuadgram)
        
        frequencyQuadgramDF <- computeProbability(frequencyQuadgramDF, frequencyUnigramDF)
        gFile <- freqFileNames(fName,"quad")
        
        write.csv(frequencyQuadgramDF,gFile)
}
