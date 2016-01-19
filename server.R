
#Data Science Capstone 

library(shiny)
library(scales)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rCharts)
#library(leaflet)
library(RColorBrewer)
library(DT)
library(pryr)
library(lineprof)
library(gdata)
source("TextPredictor.R")

#This is the data source
container <- NGramContainer$new()
container$restore()  #Load the data from the file system.

messageData <-
        data.frame(
                from = c("brettt@slhs.org","wilfordtr@slhs.org"),message = c("Hey!","What?")
        )



function(input, output,session) {
        print(session)
        predictor <- NGramPredictor$new(source=container) #This predictor is specific to the user
        autoInvalidate <- reactiveTimer(3000,session = session)
#         observe({
#                 autoInvalidate()
#         })
        
        performanceValues <- reactiveValues(predict.time = 0)
        
        #This is a test
        predictedWords <- reactive({
                textEntered()
        })
        
        predictedWordTable <- reactive({
                performanceValues$predict.time <- system.time({
                      results <- predictor$predictNextWord(  textEntered())
                        
                 })
                results
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
                        "R Memory", humanReadable(mem_used(),standard = "SI"), subtitle = "Delta",icon = icon("arrow-up"),color = "red" ,width = 3
                )
        })
        output$predictInfo <- renderInfoBox({
     
                infoBox(
                        "Performance", paste(
                                round(performanceValues$predict.time["elapsed"],2), "sec"
                        ), subtitle = "Average",icon = icon("arrow-up"),color = "red" ,width = 3
                )
        })
        output$textPredictionOut <- renderText({
                predictedWords()
                
        })
       
        
        
        output$predictResults <- DT::renderDataTable({
                data <- predictedWordTable()
                if (nrow(data) == 0)
                        return(datatable(data))
                
                datatable(
                        data , extensions = c("ColReorder",'ColVis','Responsive'), options = list(
                                dom = 'RC<"clear">lfrtip',pageLength = 20, autoWidth = TRUE,
                                colVis = list(
                                        exclude = c(0, 1), activate = 'mouseover'
                                )
                        )
                )
                
        })
}
