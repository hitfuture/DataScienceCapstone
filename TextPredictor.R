#TextPredictor class
#Utilize the RC Class model to create several types of TextPredictors

TextPredictor <- setRefClass("TextPredictor",
                       fields = c("source"),
                       methods = list(
                               predictNextWord = function(phrase) {
                                       return("<Unknown>")
                               }
                       )
)
TextPredictor$accessors(c("source"))
#Abstract subclass NGramPredictor handles maintaining the NGram lists
NGramPredictor <- setRefClass("NGramPredictor",
                             contains = "TextPredictor",
                             fields = c("ngrams")
                             )
NGramPredictor$accessors(c("ngrams"))

#Class NGram
#Abstract class that handles computing NGrams
#Question - should I make this a subclass of data.table?  
#Benefits - able for view it in the Rstudio data frame viewer.
#Issues - attributes could be reused
#I think I will encapsulate the data.table in the instance of the class.  This may change over time

NGram <- setRefClass("NGram",
                     fields = c("data"),
                     methods = list(
                             prune = function() {stop("The class does not have prune method defined")}
                             
                     ))
NGram$accessors(c("data"))
#UniGram does not have a parent.  It is the keeper of the words
UniGram <- setRefClass("UniGram",
                      contains = "NGram",
                     fields = list(),
                     methods = list(
                             getParent = function() {NULL}
                             
                     ))
#Abstract class that knows it has a parent
TwoPlusGram <- setRefClass("TwoPlusGram",
                        contains = "NGram",
                       fields = list(parent="NGram"),
                       methods = list(
                         
                       ))
TwoPlusGram$accessors(c("parent"))
BiGram <- setRefClass("BiGram",
                      contains = "TwoPlusGram",
                      fields = list(),
                      methods = list(
                              
                      ))
TriGram <- setRefClass("TriGram",
                      contains = "TwoPlusGram",
                      fields = list(),
                      methods = list(
                              
                      ))
QuadGram <- setRefClass("QuadGram",
                       contains = "TwoPlusGram",
                       fields = list(),
                       methods = list(
                               
                       ))
#NGramContainer class is responsible for adding in all of the various N-Grams, storing, and processing them.   

NGramContainer <- setRefClass("NGramContainer",
                              fields=list(ngrams="list"),
                              methods = list(
                                      setUniGram = function(ngram) {
                                              ngrams["uniGram"]<<-ngram
                                      },
                                      setBiGram = function(ngram) {
                                              ngrams["biGram"]<<-ngram
                                      },
                                      setTriGram = function(ngram) {
                                              ngrams["triGram"]<<-ngram
                                      },
                                      setQuadGram = function(ngram) {
                                              ngrams["quadGram"]<<-ngram
                                      },
                                      #Getters   
                                      getUniGram = function(){ngrams[["uniGram"]]},
                                      getBiGram = function(){ngrams[["biGram"]]},
                                      getTriGram = function(){ngrams[["triGram"]]},
                                      getQuadGram = function(){ngrams[["quadGram"]]}
                                      
                                      
                                      
                              ))
uni <- UniGram$new(data=freq.uni)

uni$getParent()
bi <- BiGram$new(data=freq.bi,parent=uni)
bi$getParent()
 
tri <- TriGram$new(data=freq.tri,parent=bi)
tri$getParent()
quad <- QuadGram$new(data=freq.quad,parent=tri)
quad
container <- NGramContainer$new()
container$setUniGram(uni)
container$setBiGram(bi)
container$setTriGram(tri)
container$setQuadGram(quad)
container
#Examples
predict1 <- TextPredictor$new(source=container)
predict1$predictNextWord("This is a test")

predictNgram <- NGramPredictor$new(source=container)
predictNgram$setNgrams(list(uni=freq.uni,bi=freq.tri,tri=freq.tri,quad=freq.quad))
predictNgram$getNgrams()
predictNgram$getSource()
predictNgram$predictNextWord("this is the word")
