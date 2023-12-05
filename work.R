library(shiny)
library(readr)
library(ggplot2)
library(dplyr)

joined_dataset <- read_csv("~/Downloads/DataWrangling/joined_dataset.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Climate Change and Food Supply Chain Analysis"),
  
  # Tabs for different sections
  tabsetPanel(
    tabPanel("Introduction",
             tags$div(
               h2("Welcome to our Climate Change and Food Supply Chain Analysis"),
               p("Explore the impact of climate change on agriculture and the resilience of the food supply chain."),
               img(src = "~/Downloads/FinalDeliverable/Climate-Smart-Food-Supply-chain-disruptions-and-water-scarcity-in-focus.jpg", height = 200, width = 300),  # Replace with your image
               p("For more details, check out our "),
               a("GitHub repository", href = "https://github.com/INFO-201-Fall-2023-Final/final-projects-thuyng25.git")
             )
    ),
    
    # Tab for Climate Impact on Agriculture
    tabPanel("Climate Impact on Agriculture",
             plotOutput("agriculture_chart")
    ),
    
    # Tab for Supply Chain Resilience
    tabPanel("Supply Chain Resilience",
             plotOutput("resilience_chart")
    ),
    
    # Tab for Adaptive Strategies
    tabPanel("Adaptive Strategies",
             plotOutput("strategies_chart")
    ),
    
    # Summary Takeaways & About Page
    tabPanel("Summary & About",
             h2("Key Takeaways"),
             p("Climate change poses significant challenges to the food supply chain."),
             p("Our analysis reveals key strategies for adaptation and resilience."),
             hr(),
             h2("About the Team"),
             p("This analysis was conducted by [Thuy Nguyen], [Your Name], and [Your Name]."),
             p("Data sources: [List your data sources]")
    )
  )
)

# Define server
server <- function(input, output) {
  # Server logic for Climate Impact on Agriculture
  output$agriculture_chart <- renderPlot({
    # Replace with your actual plot based on the dataset
    ggplot(joined_dataset, aes(x = Country, y = Amount, color = Year)) +
      geom_line() +
      labs(title = "Climate Impact on Agriculture",
           x = "Country",
           y = "Amount",
           color = "Year") +
      theme_minimal()
  })
  
  # Server logic for Supply Chain Resilience
  output$resilience_chart <- renderPlot({
    # Replace with your actual plot based on the dataset
    # ...
  })
  
  # Server logic for Adaptive Strategies
  output$strategies_chart <- renderPlot({
    # Replace with your actual plot based on the dataset
    # ...
  })
}

# Run the app
shinyApp(ui, server)
