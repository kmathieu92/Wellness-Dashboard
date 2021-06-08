#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(flexdashboard)
library(shiny)
library(ggplot2)
library(tidyverse)


wellness <- as_tibble(read_csv("/Users/kalebmathieu/Desktop/Wellness Dashboard/Likert_Dat_Cleaned.csv", 
                               col_types = cols(HAPPY_1 = col_integer(), 
                                                HAPPY_2 = col_integer(), HAPPY_3 = col_integer(), 
                                                HAPPY_4 = col_integer(), ANX_1 = col_integer(), 
                                                ANX_2 = col_integer(), ANX_3 = col_integer(), 
                                                ANX_4 = col_integer(), DEP_1 = col_integer(), 
                                                DEP_2 = col_integer(), DEP_3 = col_integer(), 
                                                DEP_4 = col_integer(), DEP_5 = col_integer(), 
                                                LONE_1 = col_integer(), LONE_2 = col_integer(), 
                                                LONE_3 = col_integer(), LONE_4 = col_integer(), 
                                                LONE_5 = col_integer(), SOCANX_1 = col_integer(), 
                                                SOCANX_2 = col_integer(), SOCANX_3 = col_integer(), 
                                                CLASS = col_character(), DISTANCE = col_character(), 
                                                GENDER = col_character(), GENDER_4_TEXT = col_character(), 
                                                TRANS = col_character(), SEX_ORIENT_1 = col_character(), 
                                                SEX_ORIENT_2 = col_character(), SEX_ORIENT_3 = col_character(), 
                                                SEX_ORIENT_4 = col_character(), SEX_ORIENT_5 = col_character(), 
                                                SEX_ORIENT_6 = col_character(), RACETHN = col_character(), 
                                                ETHNICITY = col_character(), RACE_1 = col_character(), 
                                                RACE_2 = col_character(), RACE_3 = col_character(), 
                                                RACE_4 = col_character(), RACE_5 = col_character(), 
                                                PAREDU = col_character(), GPA = col_character()), 
                               na = "NA"))

dataset <- wellness

    

sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(dataset),
            value=min(1000, nrow(dataset)), step=500, round=0)


selectInput('x', 'X', names(dataset))
selectInput('y', 'Y', names(dataset), names(dataset)[[2]])
selectInput('color', 'Color', c('None', names(dataset)))


selectInput('facet_row', 'Facet Row',
            c(None='.', names(wellness[sapply(wellness, is.character)])))
selectInput('facet_col', 'Facet Column',
            c(None='.', names(wellness[sapply(wellness, is.character)])))



dataset <- reactive({
    wellness[sample(nrow(wellness), input$sampleSize),]
})

renderPlot({
    p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()
    
    if (input$color != 'None')
        p <- p + aes_string(color=input$color)
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
        p <- p + facet_grid(facets)
    
    
    p <- p + geom_boxplot()
    
    print(p)
})
