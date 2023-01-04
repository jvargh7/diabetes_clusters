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
library(tidyverse)
source("var_list.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    titlePanel("Diabetes Phenotypes: An interactive dashboard"),
    tags$a(href="https://www.sciencedirect.com/science/article/pii/S1751991822001516", 
           "Varghese and Narayan 2022 Primary Care Diabetes"),
    
    # Sidebar panel
    
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = 'xvar',label = '',choices = names(continuous_vars)[1:10],selected = "BMI"),
            selectInput(inputId = 'yvar',label = '',choices = names(continuous_vars)[2:11],selected = "HbA1c (%)"),width = 3),
        
        # Show the scatterplot
        mainPanel(plotOutput("scatter1",width = 800,height=600))
    )
))
