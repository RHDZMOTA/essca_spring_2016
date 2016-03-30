library(shiny)
library(ggplot2)
library(tibble)
library(tidyr)

# functions
capital_output <- function(I, GNP, AG) 100 * I / (GNP * AG)
saving_rate    <- function(I, GNP) I/GNP
growth_rate1   <- function(y0, i, b, t) y0 * exp(t * i / b)
growth_rate2   <- function(H0, i1, i2, b1, b2, y1, y2, t){
  lambd1 <- i1 / b1
  lambd2 <- i2 / b2
  gr <- H0 * exp(lambd1 * t) / (b2*(lambd1 - lambd2)) +
    (y2 - H0 / (b2 * (lambd1 - lambd2))) * exp(lambd2 * t)
  return(gr)
}
lgrowth_rate2  <- function(H0, i1, i2, b1, b2, y1, y2, t){
  lambd1 <- i1 / b1
  lambd2 <- i2 / b2
  gr <- - H0 * exp(lambd1 * t) / (b2*(lambd1 - lambd2)) +
    (y2 + H0 / (b2 * (lambd1 - lambd2))) * exp(lambd2 * t)
  return(gr)
}
growth_rate    <- function(x, present) ((x[2] / present) ^ (1/x[1]) - 1) * 100
# x[1] <- n; x[2] <- final

ui <- shinyUI(fluidPage(
  fluidRow(
    column(12,
           titlePanel("Simulation of Leontief growth model for disarmament"),
           "by: Rodrigo H. Mota",
           fluidRow(
             column(6,
                    tags$h4("National Product"),
                    "Measured in bil. usd",
                    fluidRow(
                      column(6, 
                             numericInput(inputId = "GNP1",
                                          label = "Group 1",
                                          value = 1205,
                                          min = 0, max = NA,
                                          step = NA)),
                      column(6,
                             numericInput(inputId = "GNP2",
                                          label = "Group 2",
                                          value = 195,
                                          min = 0, max = NA,
                                          step = NA))
                    ),
                    tags$h4("Productive Investment"),
                    "Measured in bil. usd",
                    fluidRow(
                      column(6, 
                             numericInput(inputId = "I1",
                                          label = "Group 1",
                                          value = 228,
                                          min = 0, max = 300,
                                          step = NA)),
                      column(6,
                             numericInput(inputId = "I2",
                                          label = "Group 2",
                                          value = 15,
                                          min = 0, max = 30,
                                          step = NA))
                    ),
                    tags$h4("Total Consumption"),
                    "Measured in bil. usd",
                    fluidRow(
                      column(6, 
                             numericInput(inputId = "C1",
                                          label = "Group 1",
                                          value = 871,
                                          min = 0, max = 1500,
                                          step = NA)),
                      column(6,
                             numericInput(inputId = "C2",
                                          label = "Group 2",
                                          value = 166,
                                          min = 0, max = 1500,
                                          step = NA))
                    ),
                    tags$h4("Actual growth rate"),
                    "As a percent.",
                    fluidRow(
                      column(6, 
                             numericInput(inputId = "GR1",
                                          label = "Group 1",
                                          value = 4,
                                          min = 0, max = 20,
                                          step = NA)),
                      column(6,
                             numericInput(inputId = "GR2",
                                          label = "Group 2",
                                          value = 2,
                                          min = 0, max = 20,
                                          step = NA))
                    ),
                    
                    tags$h4("Capital transfer"),
                    "Measured in bil. usd",
                    numericInput(inputId = "H0",
                                 label = "Capital transfer from G1 to G2",
                                 value = 4,
                                 min = 0, max = 102,
                                 step = NA),
                    
                    tags$h4("Years for simulation"),
                    sliderInput(inputId = "time",
                                label = "Time in years",
                                value = 10,
                                min = 0, max = 30,
                                step = 1)
             ),
             column(width = 6,
                    tags$h4("Results"),
                    plotOutput(outputId="GNP"),
                    plotOutput(outputId="GR")
           ),
           withMathJax(),
           tags$h3(helpText("$$\\text{Leontief's solution:    } Y_{2,t} = -\\frac{h Y_{1,0}}{b_2 (\\lambda_1 - \\lambda_2)} \\exp{(t \\lambda_1)} + \\Big( Y_{2,0} + \\frac{h Y_{1,0}}{b_2 (\\lambda_1 - \\lambda_2)} \\Big) \\exp{(t \\lambda_2)}$$")),
           tags$h3(helpText("$$\\text{Our solution:    } Y_{2,t} = \\frac{h Y_{1,0}}{b_2 (\\lambda_1 - \\lambda_2)} \\exp{(t \\lambda_1)} + \\Big( Y_{2,0}-\\frac{h Y_{1,0}}{b_2 (\\lambda_1 - \\lambda_2)} \\Big) \\exp{(t \\lambda_2)}$$")),
           plotOutput(outputId="comp"))
    )
  )
))




server <- function(input, output){
  
  # Plot exponential growth of GDP
  output$GNP <- renderPlot({
    time <- 1:input$time; 
    i1 <- saving_rate(input$I1, input$GNP1);
    i2 <- saving_rate(input$I2, input$GNP2);
    b1 <- capital_output(input$I1, input$GNP1, input$GR1);
    b2 <- capital_output(input$I2, input$GNP2, input$GR2);
    growth1 <- sapply(X = time, FUN = growth_rate1, y0 = input$GNP1, i = i1, b = b1);
    growth2 <- sapply(X = time, FUN = growth_rate2, 
                      H0 = input$H0,
                      i1 = i1, i2 = i2,
                      b1 = b1, b2 = b2,
                      y1 = input$GNP1, y2 = input$GNP2);
    dataset <- data_frame(Time = time, Group1 = growth1, Group2 = growth2);
    extra_row <- data_frame(Time = 0, Group1 = input$GNP1, Group2 = input$GNP2);
    dataset <- rbind(extra_row, dataset)
    dataset <- gather(dataset, Group, Growth, -Time)
    ggplot(dataset, aes(x = Time, y = Growth)) +
      facet_grid(Group~., scale="free") + 
      geom_line(aes(color = Group), size = 0.75) + theme_light() +
      xlab("Time in years") + ylab("National Product in bil.")
  })
  
  #growth_rate
  output$GR <- renderPlot({
    time <- 1:input$time; 
    i1 <- saving_rate(input$I1, input$GNP1);
    i2 <- saving_rate(input$I2, input$GNP2);
    b1 <- capital_output(input$I1, input$GNP1, input$GR1);
    b2 <- capital_output(input$I2, input$GNP2, input$GR2);
    growth1 <- sapply(X = time, FUN = growth_rate1, y0 = input$GNP1, i = i1, b = b1);
    growth2 <- sapply(X = time, FUN = growth_rate2, 
                      H0 = input$H0,
                      i1 = i1, i2 = i2,
                      b1 = b1, b2 = b2,
                      y1 = input$GNP1, y2 = input$GNP2);
    dataset <- data_frame(Time = time, Group1 = growth1, Group2 = growth2);
    growth1 <- apply(X = dataset[,c(1,2)], 1, growth_rate, present = input$GNP1);
    growth2 <- apply(X = dataset[,c(1,3)], 1, growth_rate, present = input$GNP2);
    dataset <- data_frame(Time = time, Group1 = growth1, Group2 = growth2);
    extra_row <- data_frame(Time = 0, Group1 = input$GR1, Group2 = input$GR2);
    dataset <- rbind(extra_row, dataset)
    dataset <- gather(dataset, Group, Growth_Rate, -Time);
    ggplot(dataset, aes(x = Time, y = Growth_Rate)) +
      geom_line(aes(color = Group), size = 0.75) + theme_light() +
      xlab("Time in years") + ylab("Growth Rate (%)")
  })
  
  
  # Plot comparison between the leontief's model and rhm
  output$comp <- renderPlot({
    time <- 1:input$time; 
    i1 <- saving_rate(input$I1, input$GNP1);
    i2 <- saving_rate(input$I2, input$GNP2);
    b1 <- capital_output(input$I1, input$GNP1, input$GR1);
    b2 <- capital_output(input$I2, input$GNP2, input$GR2);
    growth2 <- sapply(X = time, FUN = growth_rate2, 
                      H0 = input$H0,
                      i1 = i1, i2 = i2,
                      b1 = b1, b2 = b2,
                      y1 = input$GNP1, y2 = input$GNP2);
    lgrowth2 <- sapply(X = time, FUN = lgrowth_rate2, 
                       H0 = input$H0,
                       i1 = i1, i2 = i2,
                       b1 = b1, b2 = b2,
                       y1 = input$GNP1, y2 = input$GNP2);
    dataset <- data_frame(Time = time, Growth_rhm = growth2,
                          Growth_leontief = lgrowth2);
    extra_row <- data_frame(Time = 0, Growth_rhm = input$GNP2,
                            Growth_leontief = input$GNP2);
    dataset <- rbind(extra_row, dataset)
    dataset <- gather(dataset, Model, Growth, -Time);
    ggplot(dataset, aes(x = Time, y = Growth)) +
      #facet_grid(Group~., scale="free") + 
      geom_line(aes(color = Model), size = 0.75) + theme_light() +
      xlab("Time in years") + ylab("National Product in bil.") +
      ggtitle("National product estimated for G2 by different models")
    
  })
  
  
  }

shinyApp(ui = ui, server = server)
