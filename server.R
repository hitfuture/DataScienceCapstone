
#Data Science Capstone 

library(shiny)
library(scales)
library(ggplot2)
library(dplyr)
library(tidyr)
library(xts)
library(data.table)
library(RColorBrewer)
library(DT)
library(pryr)
library(lineprof)
library(gdata)
source("TextPredictor.R")

#This is the data source
container <- NGramContainer$new()
container$restoreDataObject(dir ="./data/store2/")
message("data read")
messageData <-
        data.frame(
                from = c("brett.r.taylor@me.com"),message = c("Hey!")
        )
iconName <- function (isNgram) {
        if(isNgram) { 
                ic <- "check"
         } else {ic <- "hand-paper-o"
         }
        ic
}

colorName <- function (isNgram) {
        if(isNgram) { 
                 col <- "green"
        } else { 
        col <- "black"}
        col
}
function(input, output,session) {
        predictionValues <- reactiveValues(count = 0,success=0)
        successRate <- reactiveValues(count = 0,success=0)
        failureRate <- reactiveValues(count = 0,success=0)
        
        
        # print(session)
        predictor <- NGramPredictor$new(source=container) #This predictor is specific to the user
        autoInvalidate <- reactiveTimer(10000,session = session)

        performanceValues <- reactiveValues(predict.time = 0)
        outcomesQuad <- reactiveValues( gramUsed = FALSE )
        outcomesTri <- reactiveValues(            
                                                   gramUsed = FALSE )
        outcomesBi<- reactiveValues(          
                                                   gramUsed = FALSE )
        outcomesUni <- reactiveValues(             
                                                   gramUsed = FALSE)
 
        
        predictedWords <- reactive({textEntered()})
        
        predictedWordResults <- reactive({
                performanceValues$predict.time <- system.time({
                      results <- predictor$predictNextWord(  textEntered())
                     
                 })
                data <- results$data 
                gram <- results$ngram
                outcomesQuad$gramUsed <- FALSE
                outcomesTri$gramUsed <- FALSE
                outcomesBi$gramUsed <- FALSE
                outcomesUni$gramUsed <- FALSE
               if( class(gram) == "QuadGram") {
                       outcomesQuad$gramUsed <- TRUE

               }
                if( class(gram) == "TriGram") {
                
                        outcomesQuad$gramUsed <-TRUE
                        outcomesTri$gramUsed <- TRUE
                       
                        
                }
                 if( class(gram) == "BiGram") {      
                         outcomesQuad$gramUsed <-TRUE
                         outcomesTri$gramUsed <- TRUE 
                        outcomesBi$gramUsed <- TRUE
                 }
                if( class(gram) == "UniGram") {      
                        outcomesQuad$gramUsed <-TRUE
                        outcomesTri$gramUsed <- TRUE 
                        outcomesBi$gramUsed <- TRUE
                        outcomesUni$gramUsed <- TRUE
                        
                }
                data
                
        })
        textEntered <- eventReactive(input$submitText, {
                message("text Entered")
                 input$textEntry
                
        })
        #This is predicotr
        predictVal <- observeEvent(textEntered(),  predictionValues$count <-predictionValues$count + 1    ) 
        
        
        wordSelected <- observeEvent(input$wordIsSelected, {
                message("word selected")
                successRate$count <-successRate$count + 1
                input$wordSelection
                
        })
        
        wordRejected <- observeEvent(input$wordIsRejected, {
                message("rejected")
                failureRate$count <- failureRate$count + 1
                input$wordSelection
                
        })
        
         
  
        
        
        output$memInfo <- renderInfoBox({
                message("mem info")
                 
                autoInvalidate()
                infoBox(
                        "R Memory", humanReadable(mem_used(),standard = "SI"),
                        subtitle = "Delta",icon = icon("arrow-up"),color = "red" ,width = 3
                )
        })
        output$performanceInfo <- renderInfoBox({
                message("performance info")
                
                infoBox(
                        "Performance", paste(
                                round(performanceValues$predict.time["elapsed"],2), "sec"
                        ), subtitle = "Average",icon = icon("arrow-up"),color = "red" ,width = 3
                )
        })
        output$predictInfo <- renderInfoBox({
                message("predictInfo start")
             #   predictionValues$success<-predictionValues$success + 1
                message("predicatInfo next")
                 infoBox(
                        "Predictions", paste(
                                predictionValues$count, 
                                "Total"
                        ), subtitle =  paste("Success:" ,
                                             successRate$count,
                                             "\nFailure:", failureRate$count
                                             
                                             ),
                                             icon = icon("arrow-right"),color = "yellow" ,width = 3
                )
        })
        output$sessionInfo <- renderInfoBox({
                
                infoBox(
                        "Information"
                         , subtitle = "Average",icon = icon("arrow-up"),color = "red" ,width = 3
                )
        })
         output$quadgram <-  renderInfoBox(  
                 {
                         ic <-  iconName(outcomesQuad$gramUsed)  
                         col <- colorName(outcomesQuad$gramUsed)
                  infoBox(
                 "Quadgram - 4", 1, subtitle = "Quadgram(4)",icon = icon(ic),color = col
        )})
        output$trigram <-   renderInfoBox({
                ic <-  iconName(outcomesTri$gramUsed)  
                col <- colorName(outcomesTri$gramUsed)
                 infoBox(
                "Trigram - 3", 1, subtitle = "Trigram(3)",icon = icon(ic),color = col
        )})
        output$bigram <-   renderInfoBox({
                ic <-  iconName(outcomesBi$gramUsed)  
                col <- colorName(outcomesBi$gramUsed)
                infoBox(
                "Bigram - 2", 1, subtitle = "Bigram(2)",icon = icon(ic),color = col
        )})
        output$unigram <-   renderInfoBox({
                ic <-  iconName(outcomesUni$gramUsed)  
                col <- colorName(outcomesUni$gramUsed)                
                infoBox("Unigram - 1", 1, subtitle = "Unigram(1)",icon = icon(ic),color = col
        )})
        
        output$textPredictionOut <- renderText({
                predictedWords()
                
        })
       
       
        
        
        output$predictResults <- DT::renderDataTable({ 
                 
                mydata  <- predictedWordResults()
                str(mydata)
                message("mydata")
                updateSelectInput(session, "wordSelection", choices = head(as.character(mydata$word),10) )
                datatable(
                        mydata , extensions = c("ColReorder",'ColVis','Responsive'), options = list( 
                                dom = 'RC<"clear">lfrtip',pageLength = 20, autoWidth = TRUE, 
                                colVis = list(
                                        exclude = c(0, 1), activate = 'mouseover'
                                )
                        )
                )
                
        })
}
