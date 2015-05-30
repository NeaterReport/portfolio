
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(scales)

dashboardPage(skin="purple",
              
  dashboardHeader(title = "JAEG MoE",
                              
    #Notification and Batches
    dropdownMenu(type = "tasks", badgeStatus = "primary",
                     taskItem(value = 50, color = "green", "Awesomeness")
    )
  ),
              
  dashboardSidebar(
                
    # Title
    textInput("caption", "Name Your App", "My Awesome Error!"),
    
    # Show current date            
    h3(textOutput(("currentDate")), align="center"),
       
    # Create side menu         
    sidebarMenu(
      menuItem("Margin of Error", icon=icon("th"),
        menuSubItem("Proportion", tabName = "moe", icon = icon("dot-circle-o")),
        menuSubItem("Mean", tabName = "mean", icon = icon("square-o"))
      ),
      menuItem("Sample Size", tabName = "sample", icon = icon("smile-o"),
              badgeLabel = "New", badgeColor = "orange")
    ),
    
    br(),
    
    # Add selector for graphical style using ggthemes
    selectInput("graphstyle", label = "Roll your style!",
                choices = c("Excel" = 1,
                            "Classic" = 2,
                            "FiveThirtyEight" = 3,
                            "Tufte" = 4,
                            "Economist" = 5,
                            "Few" = 6,
                            "Stata" = 7),
                selected = 3),
    
    br(),
    
    # Gotta have @('_')@!
    p(img(src="SnowMonkey.jpg", width=230))
    
  ),
              
  dashboardBody(
    
    # Show the custom title
    h2(textOutput("caption", container = span)),
    
    tabItems(
      tabItem(tabName = "moe",
        
        fluidRow(
            valueBoxOutput("MOE1"),
            conditionalPanel(condition = "input.fpc == true",valueBoxOutput("MOE2")),
            valueBoxOutput("n")
        ),
        
        box(width=5, title = "Calculate your Margin of Errors",
          sliderInput("nn", label="Numbers of observations:",
                      min=1, max=1000, value=400, step=10, 
                      animate = animationOptions(loop = FALSE, interval = 300)),
          sliderInput("p", "Proportion:", .00, 1, .50, .05),
          
          checkboxInput("fpc", label = strong("Correct for Sampling Proportion (FPC)"), value = TRUE),
          helpText("Check if you work with a small population"),
          
          conditionalPanel(condition = "input.fpc == true",
                           sliderInput("N",label = "Population Size:",
                                       min = 0, max = 100000, value = 10000, step = 1000))
        ),
                    
        box(width=7, title="Margin of Errors", solidHeader = TRUE, status = "primary",
                      plotOutput("MOEplot1", height = 400, click = "plot_click")
        )
      ),
                
      tabItem(tabName = "mean",
          h2("Hello"),
          
          fluidRow(
                valueBoxOutput("MOE_mean"),
                conditionalPanel(condition = "input.fpc2 == true",valueBoxOutput("MOE_mean2")),
                valueBoxOutput("std")
              ),
          
          box(width=4, title = "Calculate your Margin of Errors",
              sliderInput("n_mean", label="Numbers of observations:",
                          min=1, max=1000, value=400, step=10,
                          animate = animationOptions(loop = FALSE, interval = 300)),
              numericInput("mean", label="What is your mean?", value = 100),
              numericInput("std", label="What is your Standard Deviations?", value = 15),
              checkboxInput("fpc2", label = strong("Correct for Sampling Proportion (FPC)"),value = TRUE),
              helpText("Check if you work with a small population"),
              conditionalPanel(condition = "input.fpc2 == true",
                               sliderInput("N_mean",label = "Population Size:",
                              min = 0, max = 100000, value = 10000, step = 1000))
            ),
          
          box(width=8, title = "Margin of Errors", solidHeader = TRUE, status = "primary",
              fluidRow(
                  box(width=6,
                      plotOutput("MOE_mean_plot1", height=300)),
                  box(width=6,
                      plotOutput("MEAN_with_CI", height=300))
              )
          )
      ),
      
      tabItem(tabName = "sample",
            fluidRow(
                  infoBoxOutput("desiredmoe"),
                  infoBoxOutput("samplesize"),
                  conditionalPanel(condition = "input.fpc3 == true", infoBoxOutput("samplesizefpc"))
            ),
                          
            box(
              sliderInput("dmoe", label="What's your desired margin of Error?", 
                          min=0.001, max=.20, value=.05, step=.005,
                          animate=animationOptions(interval=300, loop=FALSE)),
                numericInput("rr", label = "What's your anticipated response rate?",
                             value = .5, min=0.01, max=1.00, step=.01),
              
                checkboxInput("fpc3", label = strong("Correct for Sampling Proportion (FPC)"), value = FALSE),
                helpText("Check if you work with a small population"),
                  
                conditionalPanel(condition = "input.fpc3 == true",
                                 sliderInput("N_s", label = "Population Size:",
                                             min = 0, max = 10000, value = 1000, step = 500)),
                
                selectInput("z", label = "What's your desired level of confidence?",
                            choices = c("99%" = 2.576,
                                        "95%" = 1.96,
                                        "90%" = 1.645,
                                        "80%" = 1.282,
                                        "50%" = 0.674),
                            selected = 1.96)
            ),
            
            box(title="Sample Size", solidHeader = TRUE, status = "primary",
                  plotOutput("MOEplot2", height = 300)
            )
        )
    )
  )
)