#TextPredictor class
#Utilize the RC Class model to create several types of TextPredictors
library(data.table)
library(tm)
library(dplyr)
library(digest)
source('buildModel.R')
TextPredictor <- setRefClass("TextPredictor",
                       fields = c("source"),
                       methods = list(
                               predictNextWord = function(phrase) {
                                       return("<Unknown>")
                               },
                               cleanPhrase = function(phrase) {
                                       tolower(phrase)
                               }
                       )
)
TextPredictor$accessors(c("source"))
#Abstract subclass NGramPredictor handles maintaining the NGram lists
NGramPredictor <- setRefClass("NGramPredictor",
                             contains = "TextPredictor",
                             fields = c("ngrams"),
                        methods = list(
                                asTerms = function(phrase) {
                                        phrase <- tolower(phrase)
                                        phrase <- removePunctuationAndNumbers(phrase)
                                        terms <- strsplit(phrase,split = " ")
                                        unlist(terms)
                                },
                                predictNextWord = function(phrase) {
                                      terms <- .self$asTerms(phrase) 
                                      message(paste("Terms:",terms))
                                      result <- source$find(terms)
                                      return(result)
                                 }
                        )
                             )
NGramPredictor$accessors(c("ngrams"))

#Class NGram
#Abstract class that handles computing NGrams
#Question - should I make this a subclass of data.table?  
#Benefits - able for view it in the Rstudio data frame viewer.
#Issues - attributes could be reused
#I think I will encapsulate the data.table in the instance of the class.  This may change over time

NGram <- setRefClass("NGram",
                     fields = c("data","dir","isCompressed" ,"isPruned"),
                     methods = list(
                             initialize = function(...,dir="./data/") {
                                     dir <<-dir
                                     isCompressed <<- FALSE
                                     isPruned <<- FALSE
                                     callSuper(...)
                             },
                             prune = function() {stop("The class does not have prune method defined")},
                             compress = function() {
                                     #This function  reduces the number of fields in the NGram data.table, and will also 
                                     # use a hash to convert text statements into numeric values.
                                     stop("The class does not have compress method defined")},
                             restoreFrom = function(file) {
                                    data <<-  fread(file,header = TRUE,verbose = TRUE,showProgress = TRUE)
                             },
                             addToContainer = function(aContainer) {
                                     aContainer$setNGram(.self$ngramPrefix(),.self)
                             },
                             fileName = function() {fileName <- paste(.self$getDir(),.self$ngramPrefix(),"term","freq.csv",sep = "")},
                             restore = function() {
                                   .self$restoreFrom(.self$fileName())  
                             },
                             ngramPrefix = function() "ngram",
                             
                             saveTo = function(file) {
                                     write.csv(data,file,row.names = FALSE,na = "")
                             },
                             save = function() {
                                    if(!file.exists(.self$getDir())){
                                            dir.create(.self$getDir())
                                    }
                                     
                                     .self$saveTo(.self$fileName())
                             }
                             
                     ))
NGram$accessors(c("data","dir"))
#UniGram does not have a parent.  It is the keeper of the words
UniGram <- setRefClass("UniGram",
                      contains = "NGram",
                     fields = list(),
                     methods = list(
                             ngramPrefix = function() "unigram",
                             getParent = function() {NULL},
                             find = function(terms) {
                                    term <- last(terms)
                                    data%>%filter(w_0 == term)
                                     
                             }
                             
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
                              ngramPrefix = function() "bigram",
                              prune = function() {
                                      
                              } ,
                              find = function(terms) {
                                      lterms <- last(terms,1)
                                      data%>%filter((w_1==lterms[1]))
                              }
                              
                      ))
TriGram <- setRefClass("TriGram",
                      contains = "TwoPlusGram",
                      fields = list(),
                      methods = list(
                         ngramPrefix = function() "trigram",
                              
                         prune = function(){
                                 
                         }    ,
                         find = function(terms) {
                                 lterms <- last(terms,2)
                                 data%>%filter(w_2==lterms[1]&(w_1==lterms[2]))
                         }
                      ))

QuadGram <- setRefClass("QuadGram",  
                       contains = "TwoPlusGram",
                       fields = list(), 
                       methods = list( 
                               ngramPrefix = function() "quadgram",
                               find = function(terms) {
                                       lterms <- last(terms,3)
                                       data%>%filter(w_3==lterms[1]&(w_2==lterms[2])&(w_1==lterms[3]))
                               }
                               
                               
                       ))
#NGramContainer class is responsible for adding in all of the various N-Grams, storing, and processing them.   

NGramContainer <- setRefClass("NGramContainer",
                              fields=list(ngrams="list"),
                              methods = list(
                                      prune = function() {stop("The class does not have prune method defined")},
                                      compress = function() {stop("The class does not have compress method defined")},
                                      
                                      setUniGram = function(ngram) {
                                              ngrams["unigram"]<<-ngram
                                      },
                                      setBiGram = function(ngram) {
                                              ngrams["bigram"]<<-ngram
                                      },
                                       setTriGram = function(ngram) {
                                              ngrams["trigram"]<<-ngram
                                      },
                                      setQuadGram = function(ngram) {
                                              ngrams["quadgram"]<<-ngram
                                      },
                                      setNGram = function(ngramType, anNGram ) {
                                              ngrams[ngramType] <<-anNGram
                                      },
                                      #Getters   
                                      getUniGram = function(){ngrams[["unigram"]]},
                                      getBiGram = function(){ngrams[["bigram"]]},
                                      getTriGram = function(){ngrams[["trigram"]]},
                                      getQuadGram = function(){ngrams[["quadgram"]]},
                                      getNGram = function(name){ngrams[[name]]},
                                      ngramInstance =function(ngramType) {
                                              if(ngramType == "unigram") return(UniGram$new())
                                              if(ngramType == "bigram") return(BiGram$new())
                                              if(ngramType == "trigram") return(TriGram$new())
                                              if(ngramType == "quadgram") return(QuadGram$new())
                                              NGram$new()
                                              
                                              
                                      },
                                      getNGramInstances = function(){
                                              .self$clear()
                                              for(n in getNGramTypes()){
                                                      ng <-.self$ngramInstance(n)
                                                      .self$setNGram(n,ng)
                                              }
                                              ngrams
                                      },
                                      clear = function() {
                                              ngrams <<- list()
                                      },
                                      save = function(dir="./data/") {
                                             for(ng in ngrams) {
                                                     ng$setDir(dir)
                                                     ng$save()
                                             }
                                      },
                                      getNGramTypes = function() {
                                              c("unigram","bigram","trigram","quadgram")
                                      },
                                      restore = function(dir="./data/") {
                                              for(ng in .self$getNGramInstances()) {
                                                      ng$setDir(dir)
                                                      ng$restore()
                                              }
                                        
                                      },
                                      find = function(terms){
                                              lstN <- min(length(terms)+1,4)
                                              ngs <- .self$ngrams[lstN:1]
                                              for(ng in ngs) { 
                                                      results <- ng$find(terms)
                                                      if(nrow(results)>0) {
                                                            return(arrange(results,desc(freq)))
                                                      }    
                                                      
                                              }
                                      }
                                     
                                      
                                      
                              ))
