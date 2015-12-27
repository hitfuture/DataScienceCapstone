
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Text Prediction"),

  # Sidebar with entry of text
  sidebarLayout(
    sidebarPanel(
      textInput("dataEntry","Enter Text" )
    ),

    # Show a plot of accuracy of the prediction
    mainPanel(
      plotOutput("textAccuracyPlot")
    )
  )
))
