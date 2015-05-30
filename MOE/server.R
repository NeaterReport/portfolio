
function(input, output) {
  
# Make title
  output$caption <- renderText({
    input$caption
  })
  
# What's Today
  output$currentDate <- renderText({as.character(Sys.Date())})
  
#----- Calculate MOE -----
  
#----- MOE for Proportion
  
  MOE <- reactive({
     data.frame(x=1.96*sqrt((input$p*(1-input$p))/input$nn),
     y=1.96*sqrt((input$p*(1-input$p))/input$nn)*sqrt((input$N-input$nn)/(input$N-1))
     )
    })
  
  # Make MOE Graph, function method
  
  n <- reactive({input$nn})
  N <- reactive({input$N})
  p <- reactive({input$p})
  
  moe_varyn_fn <- function(n) {1.96*sqrt((p()[1]*(1-p()[1]))/n)}

  moe_varyn_fpc_fn <- function(n) {1.96*sqrt((p()[1]*(1-p()[1]))/n)*(sqrt((N()[1]-n)/(N()[1]-1)))}
  
  # Plot MoE by sample size
  output$MOEplot1 <- renderPlot({
      min <- input$nn
      gg <- ggplot(data.frame(n=c(min*2/3,min*3/2)), aes(n)) +
      scale_y_continuous(labels=percent) +
      stat_function(fun=moe_varyn_fn, geom="line", color="blue") + 
      stat_function(fun=moe_varyn_fpc_fn, geom="line", color="orange") +
      xlab("Sample Size") + 
      ylab("Margin of Error")
      
    # Branch to enable choose graph style
     if(input$graphstyle == 1) {gg + theme_excel()}
      else if (input$graphstyle == 2) {gg + theme_classic()}
      else if (input$graphstyle == 3) {gg + theme_fivethirtyeight()}
      else if (input$graphstyle == 4) {gg + theme_tufte()}
      else if (input$graphstyle == 5) {gg + theme_economist()}
      else if (input$graphstyle == 6) {gg + theme_few()}
      else if (input$graphstyle == 7) {gg + theme_stata()}
    })
  
  # Create values for valueBox
  
  output$MOE1 <- renderValueBox({
    status_color <- ifelse(MOE()[1] < .051, "green", "yellow")
    valueBox(
      value = paste("+/-", sprintf("%2.2f%%", MOE()[1]*100), " "),
      subtitle = "MOE",
      icon = icon("eye"),
      color = status_color
    )
  })
  

  output$MOE2 <- renderValueBox({
    status_color <- ifelse(MOE()[2] < .051, "green", "yellow")
    valueBox(
      value = paste("+/-", sprintf("%2.2f%%", MOE()[2]*100), " "),
      subtitle = "MOE with FPC",
      icon = icon("eye"),
      color=status_color
    )
  })
  
  output$n <- renderValueBox({
    valueBox(
      value = input$nn,
      subtitle = "Sample Size",
      icon = icon("eye")
    )
  })
  

#----- MOE for Mean
  
  MOE_mean <- reactive({
    data.frame(x=1.96*input$std/sqrt(input$n_mean),
               y=1.96*input$std/sqrt(input$n_mean)*sqrt((input$N_mean-input$n_mean)/(input$N_mean-1))
    )
  })
  
  # Make MOE Graph, function method
  
  mean <- reactive({input$mean})
  std <- reactive({input$std})
  n_mean <- reactive({input$n_mean})
  N_mean <- reactive({input$N_mean}) 
  
  moe_mean_varyn_fn <- function(n) {1.96*std()/sqrt(n)}
  
  moe_mean_varyn_fpc_fn <- function(n) {1.96*std()/sqrt(n)*sqrt((N_mean()-n)/(N_mean()-1))}
  
  # Plot MoE by sample size
  output$MOE_mean_plot1 <- renderPlot({
    min <- input$n_mean
    gg <- ggplot(data.frame(n=c(min-min,min+min)), aes(n)) +
      scale_y_continuous() +
      stat_function(fun=moe_mean_varyn_fn, geom="line", color="blue") + 
      stat_function(fun=moe_mean_varyn_fpc_fn, geom="line", color="orange") +
      xlab("Sample Size") + 
      ylab("Margin of Error")
    
    # Branch to enable choose graph style
    if(input$graphstyle == 1) {gg + theme_excel()}
    else if (input$graphstyle == 2) {gg + theme_classic()}
    else if (input$graphstyle == 3) {gg + theme_fivethirtyeight()}
    else if (input$graphstyle == 4) {gg + theme_tufte()}
    else if (input$graphstyle == 5) {gg + theme_economist()}
    else if (input$graphstyle == 6) {gg + theme_few()}
    else if (input$graphstyle == 7) {gg + theme_stata()}
  })
    
  # Plot mean +/- confidence interval
  output$MEAN_with_CI <- renderPlot({
    
    df <- data.frame(mean=input$mean, se = input$std/sqrt(input$n_mean), z = c(2.576,1.96,1.645,1.282,0.674), CI=c("99%", "95%", "90%", "80%", "50%"))
    
    df$moe <- df$z*df$se
    
    limits <- aes(ymax = mean + moe, ymin=mean - moe)
    
    gg <- ggplot(df, aes(colour=CI, y=mean, x=CI)) +
      geom_point(size=5) + geom_errorbar(limits, width=0.2) + theme(legend.position="bottom")
    
    # Branch to enable choose graph style
    if(input$graphstyle == 1) {gg + theme_excel()}
    else if (input$graphstyle == 2) {gg + theme_classic()}
    else if (input$graphstyle == 3) {gg + theme_fivethirtyeight()}
    else if (input$graphstyle == 4) {gg + theme_tufte()}
    else if (input$graphstyle == 5) {gg + theme_economist()}
    else if (input$graphstyle == 6) {gg + theme_few()}
    else if (input$graphstyl3 == 7) {gg + theme_stata()}
  })  
  
  # Create value for valueBox
  
  output$MOE_mean <- renderValueBox({
    status_color <- ifelse(MOE_mean()[1] < .051, "green", "yellow")
    valueBox(
      value = round(MOE_mean()[1],3),
      subtitle = "MOE",
      icon = icon("eye"),
      color = status_color
    )
  })
  
  output$MOE_mean2 <- renderValueBox({
    status_color <- ifelse(MOE()[2] < .051, "green", "yellow")
    valueBox(
      value = round(MOE_mean()[2],3),
      subtitle = "MOE with FPC",
      icon = icon("eye"),
      color=status_color
    )
  })
  
  output$std <- renderValueBox({
    valueBox(
      value = input$std,
      subtitle = "Standard Deviation",
      icon = icon("eye")
    )
  })
  
  # Calcuate Sample Size
  
  dmoe <- reactive({input$dmoe})
  NN <- reactive({input$N_s})
  
  sample_need <- reactive({(as.numeric(input$z)^2*.25)/(dmoe()[1]^2)})
  
  sample_need_withfpc <-reactive({(sample_need()*NN()) / (sample_need()+(NN()-1))})
  
  output$desiredmoe <- renderInfoBox({
    infoBox("Desired Margin of Error", dmoe(), icon=icon("credit-card"))
  })
  
  output$samplesize <- renderInfoBox({
    infoBox("Sample Size", round(sample_need()/input$rr), icon=icon("circle"), color = "yellow")
  })
  
  output$samplesizefpc <- renderInfoBox({
    infoBox("Sample Size with FPC", round(sample_need_withfpc()/input$rr), icon=icon("square"))
  })
  
  # Make Sample Size Graph
  
  sample_varymoe_99_fn <- function(n) {(as.numeric(2.576)^2*.25)/(n^2)}
  
  sample_varymoe_95_fn <- function(n) {(as.numeric(1.96)^2*.25)/(n^2)}
  
  sample_varymoe_90_fn <- function(n) {(as.numeric(1.645)^2*.25)/(n^2)}
  
  sample_varymoe_80_fn <- function(n) {(as.numeric(1.282)^2*.25)/(n^2)}
  
  sample_varymoe_50_fn <- function(n) {(as.numeric(0.674)^2*.25)/(n^2)}
  
  # Plot Sample size by confidence level
  
  output$MOEplot2 <- renderPlot({
    gg <- ggplot(data.frame(n=c(.05, .10)), aes(n)) +
    stat_function(fun=sample_varymoe_99_fn, geom="line", color="#a1d99b") +
    stat_function(fun=sample_varymoe_95_fn, geom="line", color="#74c476") +
    stat_function(fun=sample_varymoe_90_fn, geom="line", color="#41ab5d") +
    stat_function(fun=sample_varymoe_80_fn, geom="line", color="#238b45") +
    stat_function(fun=sample_varymoe_50_fn, geom="line", color="#005a32") +
    xlab("Sample Size") + 
    ylab("Margin of Error") +
    theme_fivethirtyeight()
    
    # Branch to enable choose graph style
    if(input$graphstyle == 1) {gg + theme_excel()}
    else if (input$graphstyle == 2) {gg + theme_classic()}
    else if (input$graphstyle == 3) {gg + theme_fivethirtyeight()}
    else if (input$graphstyle == 4) {gg + theme_tufte()}
    else if (input$graphstyle == 5) {gg + theme_economist()}
    else if (input$graphstyle == 6) {gg + theme_few()}
    else if (input$graphstyle == 7) {gg + theme_stata()}
    
  })
  
}
