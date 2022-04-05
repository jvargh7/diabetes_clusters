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
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    source("var_list.R",local=TRUE)
    
    df = readRDS("data.RDS")
    
    # print(head(df))
    selectedData <- reactive({
        x_col = continuous_vars[input$xvar]
        y_col = continuous_vars[input$yvar]
        
        df2 <-  df %>%
            dplyr::filter(variable %in% c(x_col,y_col)) %>% 
            mutate(var = case_when(variable == x_col ~ "x_col",
                                   variable == y_col ~ "y_col")) %>% 
            dplyr::select(label,var,asian,group,central,lower,upper) %>% 
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
            facet_wrap(~group,nrow = 4,ncol=2) +
            theme_bw() +
            theme(legend.position = "bottom") +
            scale_color_manual(name = "",values=c("red","darkgreen"))
        
    )

})
