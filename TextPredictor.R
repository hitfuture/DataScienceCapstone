#TextPredictor class
#Utilize the RC Class model to create several types of TextPredictors
library(data.table)
library(tm)
library(dplyr)
library(digest)
source('buildModel.R')
TextPredictor <- setRefClass(
        "TextPredictor",
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
NGramPredictor <- setRefClass(
        "NGramPredictor",
        contains = "TextPredictor",
        fields = c("ngrams","gramsUsed"),
        methods = list(
                asTerms = function(phrase) {
                        phrase <- tolower(phrase)
                        phrase <-
                                removePunctuationAndNumbers(phrase)
                        terms <-
                                strsplit(phrase,split = " ")
                        unlist(terms)
                },
                predictNextWord = function(phrase) {
                        terms <- .self$asTerms(phrase)
                         result <- source$find(terms)
                        return(head(result,10))
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

NGram <- setRefClass(
        "NGram",
        fields = c("data","dir","isCompressed" ,"isPruned"),
        methods = list(
                initialize = function(...,dir = "./data/") {
                        dir <<- dir
                        isCompressed <<- FALSE
                        isPruned <<- FALSE
                        callSuper(...)
                },
                prune = function(level=1) {
                        if(nrow(data)>1000) {
                                data <<- data%>%filter(freq > level)
                        isPruned <<- TRUE
                        }
                 },
                compress = function() {
                        #This function  reduces the number of fields in the NGram data.table, and will also
                        # use a hash to convert text statements into numeric values.
                        stop("The class does not have compress method defined")
                },
                restoreFrom = function(file) {
                        data <<-
                                fread(
                                        file,header = TRUE,verbose = TRUE,showProgress = TRUE
                                )
                },
                restoreDataObjectFrom = function(file) {
                        data <<- readRDS(file)
                                 
                }, 
                addToContainer = function(aContainer) {
                        aContainer$setNGram(.self$ngramPrefix(),.self)
                },
                fileName = function(ext=".csv") {
                        fileName <-
                                paste(.self$getDir(),.self$ngramPrefix(),"term","freq",ext,sep = "")
                },
                restore = function() {
                        .self$restoreFrom(.self$fileName())
                },
                restoreDataObject = function() {
                        .self$restoreDataObjectFrom(.self$fileName(ext=".rds"))
                },
                ngramPrefix = function()
                        "ngram",
                
                saveTo = function(file) {
                        write.csv(data,file,row.names = FALSE,na = "")
                },
                save = function() {
                        if (!file.exists(.self$getDir())) {
                                dir.create(.self$getDir())
                        }
                        
                        .self$saveTo(.self$fileName())
                },
                saveDataObjectTo= function(file){
                        saveRDS(data,file = file)
                },
                saveDataObject = function() {
                        if (!file.exists(.self$getDir())) {
                                dir.create(.self$getDir())
                        }
                        saveDataObjectTo(.self$fileName(ext=".rds"))
                }
        )
)
NGram$accessors(c("data","dir"))
#UniGram does not have a parent.  It is the keeper of the words
UniGram <- setRefClass(
        "UniGram",
        contains = "NGram",
        fields = list(),
        methods = list(
                ngramPrefix = function()
                        "unigram",
                getParent = function() {
                        NULL
                },
                find = function(terms) {
                        message(paste("find Unigram:",paste(terms,collapse = ","))) 
                        term <- last(terms)
                        data 
                },
                compress = function() {
                        data$word <<- as.factor(data$word)
                        if (!isCompressed) {
                                data <<- data %>% select(-w_0,-type)
                                isCompressed <<- TRUE
                        }
                },
                
                restore = function(...) {
                        callSuper(...)
                        if (ncol(data) <= 4) {
                                isCompressed <<- TRUE
                        }
                        
                        
                },
                restoreDataObject = function(...) {
                        callSuper(...)
                        if (ncol(data) <= 4) {
                                isCompressed <<- TRUE
                        }
                        
                        
                }
                
        )
)
#Abstract class that knows it has a parent
TwoPlusGram <- setRefClass(
        "TwoPlusGram",
        contains = "NGram",
        fields = list(parent = "NGram"),
        methods = list( restoreDataObject = function(...) {
                callSuper(...)
                if (ncol(data) <= 5) {
                        isCompressed <<- TRUE
                }
                
                
        })
)
TwoPlusGram$accessors(c("parent"))
BiGram <- setRefClass(
        "BiGram",
        contains = "TwoPlusGram",
        fields = list(),
        methods = list(
                ngramPrefix = function()
                        "bigram",
                
                find = function(terms) {
                        
                        lterms <- last(terms,1)
                        message(paste("find Bigram:",paste(lterms,collapse = ","))) 
                        
                        if (isCompressed) {
                                data %>% filter(terms == paste(lterms,collapse=" "))
                        } else{
                                data %>% filter((w_1 == lterms[1]))
                        }
                },
                
                compress = function() {
                        if (!isCompressed) {
                                data <<- data %>% select(-word,-type) %>% rename(terms = w_1,word=w_0)
                                pd <- .self$getParent()$data 
                                message(str(pd))
                                #data$word <<-
                                #        factor(data$word,levels(pd[,word]))
                                isCompressed <<- TRUE
                        }
                },
                restore = function(...) {
                        callSuper(...)
                        if (ncol(data) <= 5) {
                                isCompressed <<- TRUE
                        }
                        
                        
                }
               
                
        )
)
TriGram <- setRefClass(
        "TriGram",
        contains = "TwoPlusGram",
        fields = list(),
        methods = list(
                ngramPrefix = function()
                        "trigram",
                
                 
                compress = function(...) {
                        if (!isCompressed) {
                                str(data)
                                data <<-
                                        data %>% mutate(terms = paste(w_2,w_1))%>%select(-type,-word,-w_2,-w_1)%>%rename(word = w_0)
                                isCompressed <<- TRUE
                        }
                } ,
                
                find = function(terms) {
                        lterms <- last(terms,2)
                        message(paste("find Trigram:",paste(lterms,collapse = ","))) 
                        
                        if (isCompressed) {
                                data %>% filter(terms ==  paste(lterms,collapse=" "))
                        }else {
                                data %>% filter(w_2 == lterms[1] &
                                                        (w_1 == lterms[2]))
                        }
                },
                restore = function(...) {
                        callSuper(...)
                        if (ncol(data) <= 6) {
                                isCompressed <<- TRUE
                        }
                }
        )
)

QuadGram <- setRefClass(
        "QuadGram",
        contains = "TwoPlusGram",
        fields = list(),
        methods = list(
                ngramPrefix = function()
                        "quadgram",
                find = function(terms) {
                        lterms <- last(terms,3)
                        message(paste("find Quadgram:",paste(lterms,collapse = ","))) 
                        
                        if (isCompressed) {
                                data %>% filter(terms ==  paste(lterms,collapse=" "))
                        } else {
                                data %>% filter(w_3 == lterms[1] & (w_2 == lterms[2]) &
                                                        (w_1 == lterms[3]))
                        } 
                } ,
                compress = function() {
                        if (!isCompressed) {
                                data <<-
                                        data %>% mutate(terms = paste(w_3,w_2,w_1)) %>% select(-word ,-type,-w_3,-w_2,-w_1)%>%rename(word=w_0)
                                  isCompressed <<- TRUE
                        }
                },
                restore = function(...) {
                        callSuper(...)
                        if (ncol(data) <= 5) {
                                isCompressed <<- TRUE
                        }
                }
                
                
        )
)
#NGramContainer class is responsible for adding in all of the various N-Grams, storing, and processing them.

NGramContainer <- setRefClass(
        "NGramContainer",
        fields = list(ngrams = "list"),
        methods = list(
                prune = function( level = 1) {
                         
                        for (ng in ngrams) {
                                ng$prune(level = level)
                        }
                        },
                compress = function() {
                        for (n in ngrams)
                                n$compress()
                },
                
                setUniGram = function(ngram) {
                        ngrams["unigram"] <<- ngram
                },
                setBiGram = function(ngram) {
                        ngrams["bigram"] <<- ngram
                },
                setTriGram = function(ngram) {
                        ngrams["trigram"] <<- ngram
                },
                setQuadGram = function(ngram) {
                        ngrams["quadgram"] <<- ngram
                },
                setNGram = function(ngramType, anNGram) {
                        ngrams[ngramType] <<- anNGram
                },
                #Getters
                getUniGram = function() {
                        ngrams[["unigram"]]
                },
                getBiGram = function() {
                        ngrams[["bigram"]]
                },
                getTriGram = function() {
                        ngrams[["trigram"]]
                },
                getQuadGram = function() {
                        ngrams[["quadgram"]]
                },
                getNGram = function(name) {
                        ngrams[[name]]
                },
                ngramInstance = function(ngramType) {
                        if (ngramType == "unigram")
                                return(UniGram$new())
                        if (ngramType == "bigram")
                                return(BiGram$new())
                        if (ngramType == "trigram")
                                return(TriGram$new())
                        if (ngramType == "quadgram")
                                return(QuadGram$new())
                        NGram$new()
                        
                        
                },
                getNGramInstances = function() {
                        .self$clear()
                        for (n in getNGramTypes()) {
                                ng <- .self$ngramInstance(n)
                                .self$setNGram(n,ng)
                        }
                        ngrams
                },
                clear = function() {
                        ngrams <<- list()
                },
                save = function(dir = "./data/") {
                        for (ng in ngrams) {
                                ng$setDir(dir)
                                ng$save()
                        }
                },
                saveDataObject = function(dir = "./data/") {
                        for (ng in ngrams) {
                                ng$setDir(dir)
                                ng$saveDataObject()
                        }
                },
                getNGramTypes = function() {
                        c("unigram","bigram","trigram","quadgram")
                },
                restore = function(dir = "./data/") {
                        for (ng in .self$getNGramInstances()) {
                                ng$setDir(dir)
                                ng$restore()
                        }
                        
                },
                restoreDataObject = function(dir = "./data/") {
                        for (ng in .self$getNGramInstances()) {
                                ng$setDir(dir)
                                ng$restoreDataObject()
                        }
                        
                },
                find = function(terms) {
                        lstN <- min(length(terms) + 1,4)
                        ngs <-.self$ngrams[lstN:1]
                        for (ng in ngs) {
                                results <- ng$find(terms)
                                if (nrow(results) >
                                    0) {
                                        return(list(ngram=ng,
                                                data=arrange(results,desc(freq))))
                                }
                                
                        }
                }
                
                
                
        )
)
