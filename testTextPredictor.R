#Test the TextPredictor classes

uni <- UniGram$new(data=freq.uni)
uni$save()
uni$setData(NULL) 
uni$restore()
uni$data
uni$getParent()
bi <- BiGram$new(data=freq.bi,parent=uni)
bi$getParent()
bi$save() 
bi$setData(NULL)
is.null(bi$getData() )
bi$restore()
is.null(bi$getData() )

tri <- TriGram$new(data=freq.tri,parent=bi)
tri$getParent()
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


container$save()
container$clear()
expect_equal(length(container$ngrams) , 0)
container$restore()
expect_equal(length(container$ngrams) , 4)

#Examples
predict1 <- TextPredictor$new(source=container)
predict1$predictNextWord("This is a test")

predictNgram <- NGramPredictor$new(source=container)
predictNgram$setNgrams(list(uni=freq.uni,bi=freq.tri,tri=freq.tri,quad=freq.quad))
predictNgram$getNgrams()
predictNgram$getSource()
predictNgram$predictNextWord("this is the word")
