#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Made by Vilgot Huhn, 2023.

library(shiny)

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("My Shiny App"),
    h4("How high averages require quite dramatic selection effects"),

    # Sidebar with a slider input for shape and shift of sigmoid function
    sidebarLayout(
        sidebarPanel(
            sliderInput("shift",
                        "Shift:",
                        min = -5.5, max = 5.5, value = 0),
            sliderInput("rise",
                        "Rise:",
                        min = 0.25, max = 4, value = 2)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          HTML("<p>Made in connection to this blogpost: <a href='https://open.substack.com/pub/unconfusion/p/i-dont-think-slatestarcodex-readers?r=1vkdhx&utm_campaign=post&utm_medium=web'>about improbably high self-reports of IQ</a>, but the idea is relevant in other contexts as well.</p>"),
          p("First plot represents the probability that someone at a given level finds an interest in the blog. This probability is then multiplied by the probability density function of the normal distribution."),
          fluidRow(
            splitLayout(cellWidths = c("50%", "50%"), plotOutput("distPlot"), plotOutput("newPlot"))
          ),
          textOutput("resulting.average"),
          p(""),
          p("Made by this idiot:"),
          img(src = "profilbild.png", height = 72, width = 72)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  p    <-  seq(from = -5, to = 5, by = 0.01)
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
      s <- input$shift*input$rise
      s2 <- input$rise
        x    <-  1/(1+exp(s-p*s2))
        #o 

        # draw the histogram with the specified number of bins
        plot(p,x, type = "l",
             ylab = 'interest',
             main = "Interest function", 
             xlab = "",
             panel.first = c(abline(v = -2, col = "gray"),
                             abline(v = -1, col = "gray"),
                             abline(v = 0, col = "gray"),
                             abline(v = 1, col = "gray"),
                             abline(v = 2, col = "gray")
                             ) )
      
    })
    output$newPlot <- renderPlot({
      s <- input$shift*input$rise
      s2 <- input$rise
      x    <-  1/(1+exp(s-p*s2))
      plot(p,dnorm(p)*x, type = "l",
           main = "Resulting distribution",
           xlab = "", ylab = "probability", 
           panel.first = c(abline(v = -2, col = "gray"),
                           abline(v = -1, col = "gray"),
                           abline(v = 0, col = "gray"),
                           abline(v = 1, col = "gray"),
                           abline(v = 2, col = "gray"),
                           abline(v = weighted.mean(p, dnorm(p)*x), col = "red")
                           ) ) #if we want maximum likelihood we can use abline(v = p[which.max(x*dnorm(p))], col = "red")
    })
   output$resulting.average <- renderText({
     s <- input$shift*input$rise
     s2 <- input$rise
     x    <-  1/(1+exp(s-p*s2))
     paste(c("Mean of new distribution =", round(weighted.mean(p, dnorm(p)*x), digits = 2)))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
