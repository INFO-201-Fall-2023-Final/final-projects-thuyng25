# Final Project -- Data Wrangling 
# Thuy Nguyen, Nola Fung, Le Nguyen

library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)

climate_df <- read.csv("climate change dataset.csv")
food_df <- read.csv("food security dataset.csv")

joined_df <- left_join(x = food_df, y = climate_df, by = "Year")
joined_df <- filter(joined_df, Year >= 2003)
joined_df <- filter(joined_df, Commodity != "Economic Data")

# New numerical variable `kg food per person`
food_avail_df <- filter(joined_df, Item == "Food Availability")
food_avail_df <- filter(food_avail_df, Commodity == "Total Grains/Cereals")
food_amounts <- select(food_avail_df, Amount, Year, Country)
population_df <- filter(joined_df, Item == "Total Population - Both Sexes")
populations <- select(population_df, Amount, Year, Country)

joined_df2 <- left_join(x = population_df, y = food_amounts, by = c("Year", "Country"))
joined_df2 <- mutate(joined_df2, food_per_person = (Amount.y * 10^6) / (Amount.x * 10^6)) #kg food per person

i <- left_join(x = joined_df, y = joined_df2, by = c("Year", "Country"))
joined_df$kg_food_per_person <- i$food_per_person

# New categorical variable amount ranking 
breaks <- c(-Inf, 0, 0.5, 1, Inf)

joined_df$Amount_Category <- cut(joined_df$Amount, breaks = breaks, labels = c("Low", "Medium-Low", "Medium-High", "High"))

# Create a new continuous/numerical variable
joined_df$new_numerical <- joined_df$Anomaly * 2

# Group by kg_food_per_person and calculate mean of new_numerical
summary_df <- joined_df %>%
  group_by(kg_food_per_person) %>%
  summarise(mean_numerical = mean(new_numerical), count = n())

## Creating Shiny for "joined_df"
# Define UI
ui <- fluidPage(
  titlePanel("Interactive Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_range", "Select Year Range", min = min(joined_df$Year), max = max(joined_df$Year), 
                  value = c(min(joined_df$Year), max(joined_df$Year)), step = 1),
      sliderInput("kg_food_per_person_range", "Select kg_food_per_person Range", min = min(joined_df$kg_food_per_person), 
                  max = max(joined_df$kg_food_per_person), value = c(min(joined_df$kg_food_per_person), 
                  max(joined_df$kg_food_per_person)), step = 0.1),
      selectInput("amount_category", "Select Amount Category", unique(joined_df$Amount_Category))
    ),
    
    mainPanel(
      plotOutput("scatter_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  filtered_df <- reactive({
    filter(joined_df, Year >= input$year_range[1] & Year <= input$year_range[2] &
             kg_food_per_person >= input$kg_food_per_person_range[1] & kg_food_per_person <= input$kg_food_per_person_range[2] &
             (is.null(input$amount_category) | Amount_Category == input$amount_category))
  })
  
  output$scatter_plot <- renderPlot({
    ggplot(filtered_df(), aes(x = Year, y = kg_food_per_person, color = Amount_Category)) +
      geom_point() +
      labs(title = "Scatter Plot of kg_food_per_person by Year",
           x = "Year",
           y = "kg_food_per_person",
           color = "Amount_Category") +
      scale_color_manual(values = c("Low" = "lightpink", "Medium-Low" = "grey", "Medium-High" = "lightblue", "High" = "plum2"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
