#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    source("var_list.R",local=TRUE)
    
    df = readRDS("data.RDS")
    
    # print(head(df))
    selectedData <- reactive({
        x_col = continuous_vars[input$xvar]
        y_col = continuous_vars[input$yvar]
        
        df2 <-  bind_rows(
            df %>%
            dplyr::filter(group %in% c("T2D","SAID","SIDD","SIRD","MOD","MARD")) %>% 
            dplyr::filter(variable %in% c(x_col))%>% 
                mutate(var = case_when(variable == x_col ~ "x_col")),
            df %>%
                dplyr::filter(group %in% c("T2D","SAID","SIDD","SIRD","MOD","MARD")) %>% 
                dplyr::filter(variable %in% c(y_col)) %>% 
                mutate(var = case_when(
                                       variable == y_col ~ "y_col")),
        ) %>% 
            mutate(group2 = factor(group,levels=c("T2D","SAID","SIDD","SIRD","MOD","MARD"),
                                   labels=c("Type 2 Diabetes", "Severe Auto-immune Diabetes","Severe Insulin Deficient Diabetes", 
                                            "Severe Insulin Resistant Diabetes","Mild Obesity-related Diabetes","Mild Age-related Diabetes"))) %>% 
            dplyr::select(label,var,asian,group2,central,lower,upper) %>% 
            pivot_wider(names_from = var,values_from=c(central,lower,upper))
        
        # print(head(df2))
        df2
    })
    

    output$scatter1 <- renderPlot(
        
        ggplot(data=selectedData(),aes(x = central_x_col,
                                       y = central_y_col,
                                       col = asian)) +
            geom_point(size=3) +
            geom_errorbarh(aes(xmin = lower_x_col,
                               xmax = upper_x_col)) +
            geom_errorbar(aes(ymin = lower_y_col,
                              ymax = upper_y_col)) +
            xlab(input$xvar) +
            ylab(input$yvar) +
            facet_wrap(~group2,nrow = 2,ncol=3) +
            theme_bw() +
            ggtitle("Bivariate Median and IQR")+
            
            theme(legend.position = "bottom",
                  title = element_text(size=15),
                  legend.text = element_text(size=20),
                  strip.text = element_text(size = 15)) +
            scale_color_manual(name = "",values=c("red","darkgreen"))
        
    )

})
