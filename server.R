
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

messageData <-
        data.frame(
                from = c("brett.r.taylor@me.com","wilfordtr@slhs.org"),message = c("Hey!","What?")
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
        #This is a test
        predictedWords <- reactive({
                textEntered()
        })
        
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
                input$textEntry
        })
        
        
        
        
        output$messageMenu <- renderMenu({
                # Code to generate each of the messageItems here, in a list. This assumes
                # that messageData is a data frame with two columns, 'from' and 'message'.
                msgs <- apply(messageData, 1, function(row) {
                        messageItem(from = row[["from"]], message = row[["message"]])
                })
                dropdownMenu(type = "messages", .list = msgs)
                
        })
        
  
        
        
        output$memInfo <- renderInfoBox({
                autoInvalidate()
                infoBox(
                        "R Memory", humanReadable(mem_used(),standard = "SI"),
                        subtitle = "Delta",icon = icon("arrow-up"),color = "red" ,width = 3
                )
        })
        output$predictInfo <- renderInfoBox({
     
                infoBox(
                        "Performance", paste(
                                round(performanceValues$predict.time["elapsed"],2), "sec"
                        ), subtitle = "Average",icon = icon("arrow-up"),color = "red" ,width = 3
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
                message("got results")
                
                mydata  <- predictedWordResults()
                message("mydata")
                
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
