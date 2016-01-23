
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(DT)
require(rCharts)

library(pryr)
library(lineprof)
 
#This is a function to remove the ability to use autocomplete and spell correction capabilities.   This should be in
#the shiny package instead.
textInputNoCorrect <-
        function (inputId, label, value = "", width = NULL, placeholder = NULL)
        {
                div(
                        class = "form-group shiny-input-container", style = if (!is.null(width))
                                paste0("width: ", validateCssUnit(width), ";"),
                        
                        tags$label(label, `for` = inputId), tags$input(
                                id = inputId,
                                type = "text", class = "form-control", value = value,
                                autocomplete =
                                        "off",
                                autocorrect =
                                        "off",
                                autocapitalize =
                                        "off" ,
                                spellcheck =
                                        "false" ,
                                placeholder = placeholder
                        )
                )
        }


sidebar <-   dashboardSidebar(
        h2("Word\n Predictor")  ,
        sidebarMenu(
                menuItem(
                        "Predict", tabName = "predict",selected = TRUE,icon = icon("star")
                ),
                menuItem("Config", tabName = "config",icon = icon("info")),
                menuItem("Help", tabName = "help",icon = icon("question"))
                
        ) ,
        
        h2("Back off path"),
        fluidRow(infoBoxOutput(outputId = "quadgram"))
        
        ,
        br(),
        fluidRow(infoBoxOutput(outputId = "trigram")),
        
        
        br(),
        fluidRow(infoBoxOutput(outputId = "bigram"))
        
        ,
        br(),
        fluidRow(infoBoxOutput(outputId = "unigram"))
        
        
        
)

body <-  dashboardBody(tabItems(
        tabItem(
                tabName = "predict",
                fluidRow(
                        infoBoxOutput("memInfo",width = 3),
                        infoBoxOutput("performanceInfo",width = 3),
                        
                        infoBoxOutput("predictInfo",width = 3),
                        
                        infoBoxOutput("sessionInfo",width = 3)
                        
                ),
                
                
                fluidRow(
                        box(
                                title = "Text Submission",
                                
                                textInputNoCorrect("textEntry",label = "Enter Text",width = "100%"),
                                actionButton("submitText","Predict!",icon = icon("star")),
                                color = "blue"
                        ),
                        box("Help",tags$video(src="http://vimeo.com/12345?1")
)
                        
                ),
                fluidRow(box(
                        selectInput("wordSelection", "Word", c("Choose one" = "", c("test"))),
                        title = "Text Prediction",textOutput("textPredictionOut"
                                                             ),
                        actionButton("wordIsSelected","Select Word",icon = icon("thumbs-up")),
                        actionButton("wordIsRejected","Reject Words",icon = icon("thumbs-down")),
                        helpText("Select the correct word by using the pull down menu once you have predicted the word. If you found the correct word,
                                 then push the Select Word button.  If you can't find it, push the Reject Words button.")
                        
                )),
                fluidRow(
                        box(
                                title = "Prediction Results",   DT::dataTableOutput("predictResults") ,width =
                                        12
                        )
                )
        ),
        tabItem(tabName = "config",
                
                fluidRow(
                        box(title = "Config Sessions",
                            textOutput("config1Out"), width = 12)
                ),
                fluidRow(
                        box(
                                title = "Config Targets",plotOutput("plotStoreTransactions"), width = 12
                        )
                        
                )),
        
        tabItem(tabName = "help",
                h1("Text Prediction Information"))
))

dashboardPage(dashboardHeader(title = "Data Science Capstone" ,
                              #                 dropdownMenuOutput("messageMenu"),
                              #                 dropdownMenuOutput("taskMenu"),
                              dropdownMenuOutput("notificationMenu")),
              sidebar,
              
              
              body)
