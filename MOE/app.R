## MOE Shiny Dashboard
## Design in Sweden. Made in China.
## Inspired by Emelie!
## Version 0.0.0.9000
## May 25 2015

## app.R ##
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(dplyr)

ui <- dashboardPage(skin="yellow",
  
  dashboardHeader(title = "JAEG MOE dashboard"),
  
  dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Widgets", tabName = "widgets", icon = icon("th"))
      )
    ),
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "dashboard",
              
      fluidRow(
        
        fluidRow(
          valueBoxOutput("MOE1"),
          valueBoxOutput("MOE2"),
          valueBoxOutput("n")
        ),
        
        box(
          title = "Calculate your Margin of Errors",
          sliderInput("n", "Number of observations:", 0, 2000, 400, 10),
          sliderInput("N", "Population Size:", 0, 10000, 1000, 500),
          sliderInput("p", "Proportion:", .00, 1, .50, .05)
          ),
    
        box(
          plotOutput("MOEplot", height = 250)
        )
      )
      ),
      
      tabItem(tabName = "widgets",
          h2("Inspired by Emelie!")
      )
    )
  )
)


server <- function(input, output) {
  
  data <- reactive({
    data <- data.frame(Sample=seq(1,input$n,by=1))
    data <- mutate(data, MOE1=sqrt((input$p*(1-input$p))/Sample)*1.96)
    data <- mutate(data, MOE2=sqrt((input$p*(1-input$p))/Sample)*1.96*(sqrt((input$N-input$n)/(input$N-1))))
  })
  output$MOEplot <- renderPlot({ggplot(data(),aes(MOE2, Sample)) + geom_point(size=1/2) + theme_tufte()})

  output$MOE1 <- renderValueBox({
    valueBox(
      value = paste("+/-", sprintf("%1.1f%%", data()$MOE1[input$n]*100), " "),
      subtitle = "MOE",
      icon = icon("eye")
    )
  })
  
  output$MOE2 <- renderValueBox({
    valueBox(
      value = paste("+/-", sprintf("%1.1f%%", data()$MOE2[input$n]*100), " "),
      subtitle = "MOE with FPC",
      icon = icon("eye")
    )
  })
  
  output$n <- renderValueBox({
    valueBox(
      value = input$n,
      subtitle = "Sample Size",
      icon = icon("diamond")
    )
  })
  
}

shinyApp(ui = ui, server = server)
