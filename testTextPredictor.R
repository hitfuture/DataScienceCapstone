#Test the TextPredictor classes

uni <- UniGram$new(data=freq.uni)
 
bi <- BiGram$new(data=freq.bi,parent=uni)
 
tri <- TriGram$new(data=freq.tri,parent=bi)

quad <- QuadGram$new(data=freq.quad,parent=tri)

container <- NGramContainer$new()
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
