#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
source("var_list.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    titlePanel("Visualizing Clusters"),
    
    # Sidebar panel
    
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = 'xvar',label = '',choices = names(continuous_vars)[1:10]),
            selectInput(inputId = 'yvar',label = '',choices = names(continuous_vars)[2:11])),
        
        # Show the scatterplot
        mainPanel(plotOutput("scatter1",width = 600,height=800))
    )
))
