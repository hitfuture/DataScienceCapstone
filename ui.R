





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
#options(RCHART_LIB = 'polycharts')



sidebar <-   dashboardSidebar(
        headerPanel("Word\n Predictor")  ,
        sidebarMenu(
                menuItem("Predict", tabName = "predict",icon = icon("dashboard")),
                menuItem("Configuration", tabName = "config",icon = icon("setting")),
                menuItem("Help", tabName = "help", icon = icon("question-circle"),
                                 badgeLabel = "new", badgeColor = "green")
                
                ) 
         ,
        h1("Information")
)
 
body <-  dashboardBody(
      
        tabItems(
                tabItem(tabName = "predict",
                        fluidRow(infoBoxOutput("memInfo",width = 3),
                                 infoBoxOutput("predictInfo",width = 3),
                                infoBox("Total Predictions", 15.3, subtitle="Deviation",icon = icon("refresh"),color="yellow",width = 3),
                                infoBox("Number of Sessions", 1, subtitle="Concurrent",icon = icon("refresh" ),color = "green",width = 3)
                               
                        ),
                        
                        
                        fluidRow(
                                box(title = "Text Submission",
                                    
                                    column(6,textInput("textEntry",label="Enter Text",width = "100%")),
                                        column(2,actionButton("submitText","Predict!",icon = icon("star"))), 
                                    color="blue",width = 8)
                        
                        ),
                        fluidRow(
                                box(title = "Text Prediction",textOutput("textPredictionOut"),width = 12
                                    ) 
                                ),
                        fluidRow( box(title="Prediciton Results",   DT::dataTableOutput("predictResults") ,width=12)
                        )
                ),
                tabItem(tabName = "config",
                        
                        fluidRow(
                                box(title = "Config Sessions",
                                    textOutput("config1Out"), width = 12)
                        ),
                        fluidRow(
                                box(title = "Config Targets",plotOutput("plotStoreTransactions"), width = 12)
                                
                        )
                ),
                
                tabItem(tabName = "help",
                        h1("Text Prediction Information")
                        
                )))

dashboardPage(
        
        dashboardHeader(title = "Data Science Capstone",
                        dropdownMenuOutput("messageMenu"),
                        dropdownMenuOutput("taskMenu"),
                        dropdownMenuOutput("notificationMenu")
                        ),
        sidebar,
        body)
