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
quad
container <- NGramContainer$new()
container$setUniGram(uni)
container$setBiGram(bi)
container$setTriGram(tri)
container$setQuadGram(quad)
container$save()
container$clear()
length(container$ngrams) == 0
container$restore()
#Examples
predict1 <- TextPredictor$new(source=container)
predict1$predictNextWord("This is a test")

predictNgram <- NGramPredictor$new(source=container)
predictNgram$setNgrams(list(uni=freq.uni,bi=freq.tri,tri=freq.tri,quad=freq.quad))
predictNgram$getNgrams()
predictNgram$getSource()
predictNgram$predictNextWord("this is the word")
