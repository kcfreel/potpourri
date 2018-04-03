## Making a new R script using Method 1 to finish the rest of the tutorial with, nestled in 
## its very own folder 

# Part I

#library(shiny)
#ui <- fluidPage()
#server <- function(input, output, session) {}
#shinyApp(ui = ui, server = server)

# After saving this ^, R Studio will recognize that this is a Shiny app.
# Green triangle + Run  will appear on the righthand side of your app.R script in R Studio. Click it.
# Not much happens, as it's an empty app, but you'll see something like "Listening on...".
# Click the red button or Escape to exit.
# You can also run the app with runApp("appP3.R").

#runApp("app2_KF.R")

# There is also another method for making an app, which is just going to File > New project > 
# > New directory and choosing to make a new shiny app, Lindsay prefers Method 1

# Make sure that I am in the correct folder at this point
setwd("/Users/kellefreel/Desktop/R_course_HIMB/mbio691dlecture10materials/shiny_tutorial/")

# We will need some other libraries as well

library(dplyr)
library(ggplot2)

## Below is a template to follow, this was how we got the other app going 
# library(shiny)
# ui <- fluidPage()
# server <- function(input, output, session) {}
# shinyApp(ui = ui, server = server)

library(shiny)
bcl <- read.csv('bclData.csv', stringsAsFactors = F) #Edited from Lindsay to have spaces around the "="
ui <- fluidPage(
        titlePanel("BC Liquor Store prices"), 
        sidebarLayout(
                sidebarPanel(sliderInput("priceInput", "Price", 
                                         min = 0, max = 100, value = c(25, 40), pre = "$"),
                             radioButtons("typeInput", "Product type",
                                          choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                                          selected = "WINE"),
                             uiOutput("countryOutput")), 
                mainPanel(
                        plotOutput("coolplot"),
                        br(), br(),
                        tableOutput("results")
                )
        )
)
server <- function(input, output, session) {
        output$countryOutput <- renderUI(
                selectInput("countryInput", "Country",
                            sort(unique(bcl$Country)), selected = "CANADA")
        )

  filtered <- reactive({
        if (is.null(input$countryInput)) {
                return(NULL)
        } # Hint: Style rule #6
        bcl %>%
                filter(Price >= input$priceInput[1],
                       Price <= input$priceInput[2],
                       Type == input$typeInput,
                       Country == input$countryInput
                )
  })

  output$coolplot <- renderPlot({
          if (is.null(filtered())) {
                  return() #  If our filtered list has nulls, don't include those
          }
          ggplot(filtered(), aes(Alcohol_Content)) +
                  geom_histogram()
  })

  output$results <- renderTable({
        filtered <-
                bcl %>%
                filter(Price >= input$priceInput[1],
                       Price <= input$priceInput[2],
                       Type == input$typeInput,
                       Country == input$countryInput)
        filtered
  })
 }

shinyApp(ui = ui, server = server)
        
#print(ui)









