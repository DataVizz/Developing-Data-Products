library(shiny)
library(datasets)
library(dplyr)

server <- (function(input, output) {
  
  # Show the cars that correspond to the filters
  output$table <- renderDataTable({
    disp_seq <- seq(from = input$disp[1], to = input$disp[2], by = 0.1)
    hp_seq <- seq(from = input$hp[1], to = input$hp[2], by = 1)
    data <- transmute(mtcars, Car = rownames(mtcars), MilesPerGallon = mpg, 
                      MonthlyGasExpense = input$dis/mpg*input$cost,
                      Cylinders = cyl, Displacement = disp, Horsepower = hp, 
                      Transmission = am)
    data <- filter(data, MonthlyGasExpense <= input$gas, Cylinders %in% input$cyl, 
                   Displacement %in% disp_seq, Horsepower %in% hp_seq, Transmission %in% input$am)
    data <- mutate(data, Transmission = ifelse(Transmission==0, "Automatic", "Manual"))
    data <- arrange(data, MonthlyGasExpense)
    data$Rank <-seq.int(nrow(data))
    data
  }, options = list(lengthMenu = c(5, 15, 30), pageLength = 30))
})

library(markdown)

ui <- fluidPage(
          (navbarPage("Best Car to Own",
                   tabPanel("Table",
                            
                          sidebarLayout(
                              sidebarPanel(
                                helpText("Estimate your monthly car usage(in miles) and gasoline expenses"),
                                numericInput('dis', 'Distance used(in miles)/month:', 250, min = 100, max = 5000),
                                numericInput('cost', 'Avg. Gasoline Price (per gallon) where you live:', 2.00, min = 1.85, max = 4.50, step=0.01),
                                numericInput('gas', 'Maximum monthly expenses($) on gasoline:', 200, min=185, max=2000),
                                checkboxGroupInput('cyl', 'Number of cylinders:', c("Four"=4, "Six"=6, "Eight"=8), selected = c(4,6,8)),
                                sliderInput('disp', 'Displacement', min=60, max=500, value=c(60,500), step=10),
                                sliderInput('hp', 'Horsepower', min=50, max=340, value=c(50,340), step=10),
                                checkboxGroupInput('am', 'Transmission:', c("Automatic"=0, "Manual"=1), selected = c(0,1))
                              ),
                              
                              
                              mainPanel(
                                dataTableOutput('table')
                              )
                            )
                          ),
                       
                     
                    tabPanel("Help",
                        mainPanel(
                          includeMarkdown("Help.md")
                                    )
                                   )
                                )
)
)

shinyApp(ui = ui, server = server)