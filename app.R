#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library('rsconnect')
library(shiny)



ui <- fluidPage(
   
   # Application title
   titlePanel("Finances"),
   
   
   fluidRow(
     column(3,
           sliderInput("initial",
                       "Initial Amount",
                       min = 0,
                       max = 100000,
                       value = 1000,
                       step = 500),
           br(),
           sliderInput("contribution",
                       "Annual Contribution",
                       min = 0,
                       max = 50000,
                       value = 2000,
                       step = 500)
     ),
     column(4,
           sliderInput("rate", 
                       "Return Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 5,
                       step = 0.1),
           br(),
           sliderInput("growth",
                       "Growth Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 2,
                       step = 0.1)
     ),
     column(5,
           sliderInput("years",
                       "Years",
                       min = 0,
                       max = 50,
                       value = 20,
                       step = 1),
           br(),
           selectInput("facet",
                       "Facet?",
                       choices = c("Yes" = 1, "No" = 2),
                       selected = 2)
           
      ),
      
      
      mainPanel(h4("Timelines"),
        plotOutput("distPlot"),
        h4("Balances"),
        tableOutput("table")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$table <- renderTable({
    future_value <- function(amount, rate, years){
      fv <- amount*(1 + rate)^years
      return(fv)
    }
  
    annuity <- function(contrib, rate, years){
      a <- contrib*((((1+rate)^years) - 1)/rate)
      return(a)
    }
    growing_annuity <- function(contrib, rate, growth, years){
      ga <- contrib*((1+rate)^years-((1+growth)^years))/(rate-growth)
      return(ga)
    }
    
    no_contrib <- c()
    
    for(q in 0:input$years){
      a <- future_value(input$initial, (input$rate * 0.01), q)
      no_contrib <- c(no_contrib, a)
    }
    
    fixed_contrib <- c()
    for(q in 0:input$years){
      a <- future_value(input$initial, input$rate * 0.01, q)
      b <- annuity(input$contribution, input$rate * 0.01, q)
      fixed_contrib <- c(fixed_contrib, a+b)
    }
    
    growing_contrib <- c()
    for(q in 0:input$years){
      a <- future_value(input$initial, input$rate * 0.01, q)
      b <- growing_annuity(input$contribution, input$rate * 0.01, input$growth * 0.01, q)
      growing_contrib <- c(growing_contrib, a+b)
    }
    
    year <- c(0:input$years)
    finances <- data.frame(year, no_contrib, fixed_contrib, growing_contrib)
    
  })
    
     
  output$distPlot <- renderPlot({
     
     future_value <- function(amount, rate, years){
       fv <- amount*(1 + rate)^years
       return(fv)
     }

     annuity <- function(contrib, rate, years){
       a <- contrib*((((1+rate)^years) - 1)/rate)
       return(a)
     }
     growing_annuity <- function(contrib, rate, growth, years){
       ga <- contrib*((1+rate)^years-((1+growth)^years))/(rate-growth)
       return(ga)
     }
     
     no_contrib <- c()
     for(q in 0:input$years){
       a <- future_value(input$initial, (input$rate * 0.01), q)
       no_contrib <- c(no_contrib, a)
     }
     
     fixed_contrib <- c()
     for(q in 0:input$years){
       a <- future_value(input$initial, input$rate * 0.01, q)
       b <- annuity(input$contribution, input$rate * 0.01, q)
       fixed_contrib <- c(fixed_contrib, a+b)
     }
     
     growing_contrib <- c()
     for(q in 0:input$years){
       a <- future_value(input$initial, input$rate * 0.01, q)
       b <- growing_annuity(input$contribution, input$rate * 0.01, input$growth * 0.01, q)
       growing_contrib <- c(growing_contrib, a+b)
     }
     
     year <- c(0:input$years)
     finances <- data.frame(year, no_contrib, fixed_contrib, growing_contrib)
   
     library(dplyr)
     library('ggplot2')
     
     NoContrib <- data.frame(
       years = finances$year,
       modality = rep("No Contribution", input$years + 1),
       amount = finances$no_contrib
     )

     
     
     FixedContrib <- data.frame(
       years = finances$year,
       modality = rep("Fixed Contribution", input$years + 1),
       amount = finances$fixed_contrib
     )
     
     GrowingContrib <- data.frame(
       years = finances$year,
       modality = rep("Growing Contribution", input$years + 1),
       amount = finances$growing_contrib
     )
     finances2 <- rbind(NoContrib, FixedContrib, GrowingContrib)
   
     if(input$facet == 2){
       ggplot(data = finances2, aes(x = years, y = amount))+
         geom_line(aes(color = modality))+
         geom_point(aes(color = modality))+
         ggtitle("Three modes of investing")
     }
     
     else{
       ggplot(data = finances2, aes(x = years, y = amount))+
         geom_area(aes(color = modality, fill = modality, alpha = .2))+
         facet_grid(. ~ modality)+
         ggtitle("Three modes of investing")
     }
  
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

