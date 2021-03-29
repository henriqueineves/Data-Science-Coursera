library(shiny)
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Statistical Sample Size (With graph!)"),
    sidebarLayout(
        sidebarPanel(h4("Input data for Power calculation:"),
            selectInput("alpha", "Select your value of Alpha:", choices = c(0.05, 0.01, 0.001)),
            sliderInput("power", "Select the desired Power of your test (%):", min = 50, max = 100, value = 80),
            numericInput("pop_mean", "Insert the control mean:", value = 10, step = 0.001),
            numericInput("smp_mean", "Insert the sample mean:", value = 1, step = 0.001),
            numericInput("sigma", "Insert the Standard Deviation of your sample", value = 0, step = 0.001),
            checkboxInput("plot_power", "Plot Power at the curve?"),
            checkboxInput("plot_n", "Plot necessary sample size at the curve?"),
            submitButton("Calculate Sample Size!")
            ),
        mainPanel(h3("Power curve plot:"),
                  plotOutput("plot"),
                  h4("Your test should gave a sample size of at least:"),
                  textOutput("smp_size")
                  )
    )
)
server <- function(input, output) {
    smp_size <- reactive({
        aph <- as.numeric(input$alpha)
        pwr <- input$power/100
        ((qnorm(pwr) + qnorm(1-(aph/2))) ** 2) * (input$sigma**2)/
            (abs(input$pop_mean - input$smp_mean))**2
    })
    output$smp_size <- renderText({ceiling(smp_size())})
    
    output$plot <- renderPlot({
        aph <- as.numeric(input$alpha)
        pwr <- 0
        n <- 1
        n_list <- c(); power_list <- c()
        while (pwr < 0.999){
            pwr <- pnorm(-qnorm(1 - (aph/2)) + ((abs(input$pop_mean - input$smp_mean) * sqrt(n))/input$sigma))
            n_list <- c(n_list, n)
            power_list <- c(power_list, pwr)
            n <- n + 1
        }
        a <- plot(n_list, (power_list*100), type = "l", xlab = "Sample Size", ylab = "Power (%)")
        if (input$plot_power == TRUE){
            a <- abline(h = input$power, col = "red", lwd = 2)
        }
        if (input$plot_n == TRUE){
            a <- abline(v = smp_size(), col = "blue", lwd = 2)
        }
        a
    })
    }

# Run the application 
shinyApp(ui = ui, server = server)
