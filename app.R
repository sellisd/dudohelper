#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(scales)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Dudo stats helper"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("one", "one", 0, min = 0, max = 5, step=1),
            numericInput("two", "two", 0, min = 0, max = 5, step=1),
            numericInput("three", "three", 0, min = 0, max = 5, step=1),
            numericInput("four", "four", 0, min = 0, max = 5, step=1),
            numericInput("five", "five", 0, min = 0, max = 5, step=1),
            numericInput("six", "six", 0, min = 0, max = 5, step=1),
            sliderInput("number_of_dice",
                        "number of dice:",
                        min = 1,
                        max = 30,
                        value = 20)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("mystats"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #' Conditional probability calculator
  #'
  #' @param number_of_dice Total number of dice in the game (all players summed)
  #' @param k Number of dice with a given side up
  #' @param side Side of dice facing up in the set (1,2,3,4,5,6)
  #' @param roll My roll of dice a named vector of the form:
  #'             c('1'=0, '2'=3, '3'=1, '4'=1, '5'=0, '6'=0)
  #'
  #' @return expected probability of at least k dice have side facing up given the\
  #'         dice roll
  #' @export
  #'
  #' @examples
  #' roll <- c('1'=0, '2'=3, '3'=1, '4'=1, '5'=0, '6'=0)
  #' number_of_dice= 25
  #' probs <- conditional(number_of_dice, c(1:number_of_dice), 2, roll)
  conditional <- function(number_of_dice, k, side, roll){
    # the expected probabilities given my dice
    if(side == 1){
      k <- k - roll[side]
      p <- 1/6
    }else{
      k <- k - roll[side] - roll['1']
      p <- 2/6
    }
    1-pbinom(k-1, number_of_dice - sum(roll), p)
  }
  
  data<-reactive({
    roll <- c('1'=input$one, '2'=input$two, '3'=input$three, '4'=input$four, '5'=input$five, '6'=input$six)
    probs <- conditional(input$number_of_dice, c(1:input$number_of_dice), 2, roll)
    df <- tibble(
      x=c(1:input$number_of_dice),
      one=conditional(input$number_of_dice, c(1:input$number_of_dice), 1, roll),
      two=conditional(input$number_of_dice, c(1:input$number_of_dice), 2, roll),
      three=conditional(input$number_of_dice, c(1:input$number_of_dice), 3, roll),
      four=conditional(input$number_of_dice, c(1:input$number_of_dice), 4, roll),
      five=conditional(input$number_of_dice, c(1:input$number_of_dice), 5, roll),
      six=conditional(input$number_of_dice, c(1:input$number_of_dice), 6, roll),
    )
    df %>% pivot_longer(-x)
  })
  
    output$mystats <- renderPlot({
      ggplot(data(), aes(
        x = x,
        y = value,
        group = name,
        color = name
      )) +
        geom_line(aes(linetype = name)) +
        geom_point() + xlab("Number of dice with side") + 
        ylab("Probability of at least X") + 
        scale_x_continuous(breaks=breaks_pretty(),limits=c(0,10))
      # improvements for graph :
      # - sort legends
      # - use emoji for dice sides
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
