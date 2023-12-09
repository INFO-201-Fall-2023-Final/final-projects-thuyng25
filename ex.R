library(shiny)
library(ggplot2)
library(dplyr)

joined_df <- read_csv("~/Downloads/DataWrangling/joined_dataset.csv")

# Define UI
ui <- fluidPage(
  # Custom HTML styling
  tags$style(HTML("
    h2 {
            background-color: #F0F8FF;
            color: #808080;
            }")),
  
  tags$style(HTML("
    h3 {
            background-color: #F0F8FF;
            color: #808080;
            }")),
  
  titlePanel("Interactive Shiny App for INFO201 Final"),
  
  # Tabs for different sections
  tabsetPanel(
    tabPanel("Introduction",
      tags$div(
        h2("Welcome to our Climate Change and Food Supply Chain Analysis"),
          p("In this interactive analysis, we explore the intricate relationship between climate change, agriculture, and food availability. 
          Our goal is to shed light on the impact of environmental changes on food production, distribution, and resilience."),
               
        # Why it Matters
        h3("Why Does It Matter?"),
         p("Climate change poses significant challenges to our planet, affecting various aspects of our lives. One critical area is the food 
          supply chain. Understanding food availability is vital for developing strategies to ensure food security in the face of changing environmental conditions."),
               
        # Data Sources
        h3("Data Sources"),
         p("To answer these questions, we've gathered and analyzed data from diverse sources, including climate datasets and food supply chain
          statistics. By combining and visualizing this information, we aim to provide a comprehensive view of the complex dynamics at play."),
               
        # Additional Flare - Image
        img(src = "/Users/tn/Downloads/FinalDeliverable/Climate-Smart-Food-Supply-chain-disruptions-and-water-scarcity-in-focus.jpg", 
        height = 300, width = 500, style = "margin-top:20px; margin-bottom:20px;"),
               
        # Additional Flare - GitHub Link
        h3("Explore Further"),
          p("For a deeper dive into our analysis, explore our GitHub repository. Check out the code, and contribute to the discussion."),
          a("GitHub Repository", href = "https://github.com/INFO-201-Fall-2023-Final/final-projects-thuyng25.git",
          style = "color:slategray; text-decoration:underline;"),
               
        # Additional Flare - External Link
        h3("External Resources"),
          p("Expand your understanding of climate-smart food supply chains by exploring additional resources."),
          a("Learn More", href = "https://climatechange.chicago.gov/climate-impacts/climate-impacts-agriculture-and-food-supply#:~:text=Moderate%20warming
                                  %20and%20more%20carbon,quality%20of%20their%20food%20supply.",
          style = "color:slategray; text-decoration:underline;"),
      )
    ),
  
    # Scatter Plot Tab
    tabPanel("Production over Time",
      sidebarLayout(
        sidebarPanel(
          sliderInput("year_range", "Select Year Range", min = min(joined_df$Year), max = max(joined_df$Year), 
                      value = c(min(joined_df$Year), max(joined_df$Year)), step = 1),
          sliderInput("kg_food_per_person_range", "Food Production Amount", min = min(joined_df$kg_food_per_person), max = max(joined_df$kg_food_per_person), 
                      value = c(min(joined_df$kg_food_per_person), max(joined_df$kg_food_per_person)), step = 0.1),
          selectInput("amount_category", "Select Amount Category", unique(joined_df$Amount_Category))
    ),
               
      mainPanel(
        h2("Exploring the Impact of Food Production:"),
          p("In examining the dataset, we've closely tracked the changes in food production over the years, considering the potential impact of climate change. 
          Our analysis reveals fluctuations in the amount of food produced per person, suggesting a possible correlation with evolving climate
          conditions. Some years show noticeable shifts, which may be influenced by changes in the climate. Understanding these trends is crucial for recognizing 
          challenges and opportunities in the food supply chain. This insight helps us take a more informed and adaptive approach to address the effects of climate 
          change on global food security. Our exploration of food production over time, in the context of climate change, is a key element in understanding the 
          dynamics within the agricultural landscape."),
                   
        h3("Key Takeaways and Analysis:"),
          p("As you navigate through the data, aim to identify key takeaways. Consider the correlation between food availability and climate change thoughout time. 
          Assess whether certain temperature patterns coincide with changes in food production. This analysis contributes to a deeper understanding of the potential 
          impact of climate change on agriculture."),
                   
        h3("Interactive Exploration:"),
          p("Feel free to interact with the plots dynamically. Click and drag to zoom in on specific regions of interest. Hover over data points to view detailed
          information, such as food availability statistics thoughout the years. This interactive exploration allows you to glean more insights from the data."),
 
        h3("Interactive Guide:"),
          p("Begin your exploration by adjusting the 'Food Production Amount' slider. This allows you to observe how production amount have varied over different years.
            Production amount represent deviations throughout time"),
          p("Simultaneously, utilize the 'Select Year Range' slider to explore trends in food availability. This interactive feature enables you 
            to correlate temperature anomalies with the production and availability of food. Consider how these factors may be interlinked and influence each other."),
          p("Refine your analysis by selecting a specific 'Amount Category' from the dropdown menu. This step allows you to observe how climate variations may affect 
            the distribution of food production across intensity levels."),
                   
        h3("Integrating Insights with Other Tabs:"),
          p("For a comprehensive analysis, integrate insights from this tab with data from other sections, such as the 'Tab2' and 'Tab3' This holistic approach
          ensures a thorough understanding of the complex interactions between changes of food availability over time due to climate change."),
                   
        plotOutput("scatter_plot"),
        
        )
      )
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
  filtered_df <- reactive({
    filter(joined_df, 
           Year >= input$year_range[1] & Year <= input$year_range[2] &
             kg_food_per_person >= input$kg_food_per_person_range[1] & 
             kg_food_per_person <= input$kg_food_per_person_range[2] &
             (is.null(input$amount_category) | Amount_Category == input$amount_category))
  })
  
  output$scatter_plot <- renderPlot({
    ggplot(filtered_df(), aes(x = Year, y = kg_food_per_person, color = Amount_Category)) +
      geom_point() +
      labs(title = "Scatter Plot of Amount in Kg per Person by Year",
           x = "Year",
           y = "Amount in Kg per Person",
           color = "Amount_Category") +
      scale_color_manual(values = c("Low" = "#EEA9B8", 
                                    "Medium-Low" = "grey", 
                                    "Medium-High" = "lightblue", 
                                    "High" = "#CDB5CD"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
