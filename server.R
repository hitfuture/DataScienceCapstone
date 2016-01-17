

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(scales)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rMaps)
library(rCharts)
#library(leaflet)
library(RColorBrewer)
library(maps)
library(mapproj)
library(DT)
library(pryr)
 library(lineprof)
library(gdata)

messageData <- data.frame(from=c("brettt@slhs.org","wilfordtr@slhs.org"),message=c("Hey!","What?"))


# if (packageVersion('shiny') > '0.7') {
#         library(shiny)
#         runGitHub("shiny-examples", "rstudio", subdir = "012-datatables")
# }

#require(devtools)
#install_github('ramnathv/rCharts@dev')
#install_github('ramnathv/rMaps')
#devtools::install_github('rstudio/shinyapps')
#devtools::install_github('rstudio/DT')
#install.packages("leaflet")

function(input, output,session) {
        print(session)
   #Reactive Functions
        autoInvalidate <- reactiveTimer(1000,session = session)
observe({
        autoInvalidate()
})



performanceValues <- reactiveValues(predict.time = 0)

#This is a test
predictedWords <- reactive({
         performanceValues$predict.time <- system.time({
         Sys.sleep(rnorm(1,.5,.3))  
         })
        textEntered()
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
    
  }
  )
  
 tasks <- list( taskItem(value = 90, color = "green",
           "Documentation"
  ),
  taskItem(value = 17, color = "aqua",
           "Project X"
  ),
  taskItem(value = 75, color = "yellow",
           "Server deployment"
  ),
  taskItem(value = 80, color = "red",
           "Overall project"
  )
  )
 
 output$taskMenu <- renderMenu({
   dropdownMenu(type = "tasks", .list = tasks,badgeStatus = "success")
   
 }
 )
    
 output$urlText <- renderText({
         paste(sep = "",
               "protocol: ", session$clientData$url_protocol, "\n",
               "hostname: ", session$clientData$url_hostname, "\n",
               "pathname: ", session$clientData$url_pathname, "\n",
               "port: ",     session$clientData$url_port,     "\n",
               "search: ",   session$clientData$url_search,   "\n"
         )
 })
 output$memInfo <- renderInfoBox({
         autoInvalidate()
         infoBox("R Memory", humanReadable(mem_used(),standard="SI"), subtitle="Delta",icon = icon("arrow-up"),color = "red" ,width = 3)
 })
 output$predictInfo <- renderInfoBox({
          infoBox("Prediction Info", paste(round(performanceValues$predict.time["elapsed"],2), "sec"), subtitle="Average",icon = icon("arrow-up"),color = "red" ,width = 3)
 })
 output$textPredictionOut <- renderText({
         predictedWords()
        
 })
  plotUtilizationByTime <-  renderPlot({
    laterst<-store.transactions%>%filter(StartedOnDate > as.Date("2015-07-16"))
    ggplot(laterst, aes(x=StartedOnDate,fill=StartedOnDayOfWeek)) + 
      geom_bar() + 
      scale_x_date(breaks=date_breaks(width="7 day"))+
      theme_bw()+
      theme( axis.text.x = element_text( angle = 45))    
  })
  
  output$plotAccessHistory <- renderPlot({
   
  ggplot(access.history.by.date, aes(x=launchStartDate,y=count,fill=isMicrosoftOffice)) + 
    geom_bar(stat = "identity") + 
    scale_x_date(breaks=date_breaks(width="7 day"))+
    theme_bw()+
    theme( axis.text.x = element_text( angle = 45))  
    
  })
  
   
  output$plotTopUser <- renderPlot({
#     topUser <-  
#       user.access.office$AD_userId[1]
#     top1.access.history <- 
#     ggplot(top1.access.history , aes(x=launchStartDate,fill=isMicrosoftOffice)) + 
#       stat_bin(binwidth=1, position="identity") + 
#     scale_x_date(breaks=date_breaks(width="7 day"))+
#       theme_bw()+
      theme( axis.text.x = element_text( angle = 45))
  })
  
  output$storeData <- DT::renderDataTable({
    data <- store.transactions[1:1000,]%>%select(-contains("guid",ignore.case=TRUE) )
    if(nrow(data) == 0)return(datatable(data))
    
    datatable(data , extensions = c("ColReorder",'ColVis','Responsive' ), options = list(dom = 'RC<"clear">lfrtip',pageLength=20, autoWidth = TRUE,
                                                                                         colVis = list(exclude = c(0, 1), activate = 'mouseover')
    ) )
    
  })
}

