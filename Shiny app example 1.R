library(shiny)
library(palmerpenguins)
library(ggplot2)
library(dplyr)

penguins <- palmerpenguins::penguins
colnames(penguins) <- c('species', 'islands', 'bill length mm', 
                        'bill depth mm', 'flipper length mm', 'body mass g',
                        'sex', 'year')

ui <- fluidPage(
    titlePanel('Task 1 - Data Visualisation Shiny app 2322312w'),
    sidebarLayout(fluid = TRUE,
                  sidebarPanel(
                      selectInput('xInput', label = 'x-axis',
                                  choices = c('islands', 'sex'),
                                  selected = 'sex'),
                      selectInput('yInput', label = 'y-axis',
                                  choices = c('bill length mm', 'bill depth mm',
                                              'flipper length mm', 'body mass g'),
                                  selected = 'bill length mm'),
                      selectInput('speciesInput', label = 'Show different species levels', 
                                  choices = c('Yes', 'No'),
                                  selected = 'No'),
                      
                      checkboxGroupInput('yearInput', label = 'Year', 
                                         choices = c('2007', '2008', '2009'))
                  ),
                  
                  mainPanel(
                      plotOutput('coolplot')
                  )
    ))




server <- function(input,output, session){
    output$coolplot <- renderPlot({
        
        if(is.null(input$yearInput)){
            if(input$speciesInput == 'Yes'){
                ggplot(data = penguins,
                       aes(x = .data[[input$xInput]], y = .data[[input$yInput]])) +
                    geom_violin(aes(colour = species)) 
                
            } else { 
                ggplot(data = penguins,
                       aes(x = .data[[input$xInput]], y = .data[[input$yInput]])) +
                    geom_violin() 
                
                
            } 
        } else if(!is.null(input$yearInput)){
            filtered.penguins <- penguins %>%
                filter(year == c(input$yearInput)) 
            
            if(input$speciesInput == 'Yes'){
                ggplot(data = filtered.penguins,
                       aes(x = .data[[input$xInput]], y = .data[[input$yInput]])) +
                    geom_violin(aes(colour = species), na.rm = FALSE) +
                    facet_wrap(~year, dir = 'v', drop = FALSE)
            } else { 
                ggplot(data = filtered.penguins,
                       aes(x = .data[[input$xInput]], y = .data[[input$yInput]])) +
                    geom_violin() +
                    facet_wrap(~year, dir = 'v', drop = FALSE) 
                
            }
        }
        
    })
}


shinyApp(ui = ui, server = server)

