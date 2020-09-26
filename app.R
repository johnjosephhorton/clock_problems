## Create clock problems for kids
## Original author: John Horton
## Shiny app: Vlad Tarko
## Date: 9/25/2020
## License: Have at it - no rights reserved.

library(shiny)
library(magrittr)
library(dplyr)
library(ggplot2)

theme_set(theme_void())

makeClocks <- function(N = 1) {
    hours <- sample(1:12, N, TRUE)
    minutes <- sample(seq(0, 60, 5), N, TRUE)
    
    AngleHour <- function(H, M) -2*pi * (H-3 + M/60)/12
    AngleMin  <- function(M)  -2*pi *  (M-15)/60
    
    h.a <- mapply(AngleHour, hours, minutes)
    m.a <- sapply(minutes, AngleMin)
    x.h <- (1/2)*cos(h.a)
    y.h <- (1/2)*sin(h.a)
    x.m <- cos(m.a)
    y.m <- sin(m.a)
    
    df <- data.frame(hours,
                     minutes,
                     x.h,
                     y.h,
                     x.m,
                     y.m,
                     index = 1:N) %>%
        mutate(correct.answer = paste0(hours,":",
                                       ifelse(minutes < 10,
                                              paste0("0",minutes),
                                              minutes)))
    
    ## Add labels for hours
    df.positions <- data.frame(
        number = 1:12,
        angle = sapply(1:12, function(h) AngleHour(h, 0))) %>%
        mutate(
            x = cos(angle),
            y = sin(angle)
        )
    
    g.base <- ggplot(data = df) +
        geom_segment(aes(x = x.h, xend = 0, y = y.h, yend = 0), size = 1) +
        geom_segment(aes(x = x.m, xend = 0, y = y.m, yend = 0)) +
        geom_text(data = df.positions, aes(x = x, y = y, label = number), size = 10) +
        #facet_wrap(~index, ncol = 4) + 
        geom_point(x = 0, y = 0) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()
        )
    
    result <- list(
        main = g.base,
        key = g.base +
            geom_text(data = df,
                      aes(x = 0.9, y = 0.9, label = correct.answer),
                      colour = "red", size = 12)
    )
    return(result)
}

ui <- fluidPage(

    sidebarLayout(
        sidebarPanel(width = 3,
            actionButton("genButton", "Generate clock"),
            checkboxInput("keyButton", "Show answer")
            #numericInput("numClocks", label = "How many clocks", value = 1, width = "150px")
        ),

        mainPanel(
           plotOutput("clocksPlot", height = "600px", width = "600px")
        )
    )
)

server <- function(input, output) {
    
    clocks <- eventReactive(input$genButton, {
        #makeClocks(input$numClocks)
        makeClocks()
    })
    
    output$clocksPlot <- renderPlot({
        if(input$keyButton) {
            clocks()$key
        } else {
            clocks()$main
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
