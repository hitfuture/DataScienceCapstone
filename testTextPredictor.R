#Test the TextPredictor classes
library(digest)
library(hash)
freq.uni <- fread("./data/trigramtermfreq.csv")
container <- NGramContainer$new()
uni <- UniGram$new(data=freq.uni)
 
bi <- BiGram$new(data=freq.bi,parent=uni)
 
tri <- TriGram$new(data=freq.tri,parent=bi)

quad <- QuadGram$new(data=freq.quad,parent=tri)

expect_equal(length(container$ngrams),0)

uni$addToContainer(container)
expect_equal(length(container$ngrams),1)

bi$addToContainer(container)
expect_equal(length(container$ngrams),2)
#Add it twice
bi$addToContainer(container)
expect_equal(length(container$ngrams),2)

tri$addToContainer(container)
expect_equal(length(container$ngrams),3)

quad$addToContainer(container)
expect_equal(length(container$ngrams),4)
container$compress()
uni$getDir()
uni$data
container$save(dir = "./data/archive/")
container$compress()
container$clear()
expect_equal(length(container$ngrams) , 0)
container$restore()
expect_equal(length(container$ngrams) , 4)

#Examples
predict1 <- TextPredictor$new(source=container)
predict1$predictNextWord("This is a test")

predictNgram <- NGramPredictor$new(source=container)
predictNgram$getNgrams()
predictNgram$getSource()
predictNgram$predictNextWord("this is the word")
predictNgram$predictNextWord("this is the")
predictNgram$predictNextWord("I'm very")
predictNgram$predictNextWord("at the end")
answer <- predictNgram$predictNextWord("the end of")
predictNgram$predictNextWord("thanks for the")
predictNgram$predictNextWord("by the end")
predictNgram$predictNextWord("the end of the day i don't have to be a great resource simple")
predictNgram$predictNextWord("by the")

strVal <- (digest("123",algo = "sha256"))
           
strVal <- paste("0x",strVal,sep="")
strtoi(substr(strVal,1,6),16L)
as.integer(strVal)
strsplit()

container <- NGramContainer$new()
container$restore(dir = "./data/store/") 
 
data.size.before <- sum(sapply(container$ngrams, function(n) {object_size(n$data)}))
container$compress()
data.size.after <- sum(sapply(container$ngrams, function(n) {object_size(n$data)}))
data.size.after/data.size.before
container$save(dir = "./data/store2/")

container <- NGramContainer$new()
container$restore(dir = "./data/store/")
container$compress()
container$prune()
container$save(dir ="./data/store2/")

predictNgram <- NGramPredictor$new(source=container)

uni<- container$getUniGram()
uni$isCompressed
bi <- container$getBiGram()
bi$isCompressed
tri <- container$getTriGram()
tri$isCompressed <- TRUE

tri$isCompressed
str(tri$data)
lapply(last(container$ngrams,3),function(x){setkey(x$data,terms)})
predictNgram$predictNextWord("this is the word")
predictNgram$predictNextWord("this is the")
predictNgram$predictNextWord("I'm very")
predictNgram$predictNextWord("at the end")
answer <- predictNgram$predictNextWord("the end of")
predictNgram$predictNextWord("thanks for the")
predictNgram$predictNextWord("by the end")
predictNgram$predictNextWord("the end of the day i don't have to be a great resource simple")
predictNgram$predictNextWord("by the")

predictNgram <- NGramPredictor$new(source=container)
View(container$getUniGram()$data)
View(tail(container$getBiGram()$data,1000))
View(tail(container$getTriGram()$data,1000))
View(tail(container$getQuadGram()$data,1000))
