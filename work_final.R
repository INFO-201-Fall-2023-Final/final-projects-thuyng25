library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)

source("FinalProject.R")
#joined_df <- read_csv("joined_dataset.csv")

#Remove a country named d'Ivo
joined_df <- joined_df[!str_detect(joined_df$Country, "d'Ivo"),]

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
      sidebarLayout(
        sidebarPanel(
          sliderInput(
            inputId = "food_per_year",
            label = "Slide to choose a year",
            min = 2003,
            max = 2013,
            value = 2003,
            sep = "",
          ),
          h6("(*Removed country Côte d'Ivoire due to string error)"),
        ),
        
        mainPanel(
          h2("Examining how Food Availability Differs By Country:"),
          p("The 'food per person' variable is more complex than it may seem at first glance, as population may serve as a proxy for other factors. Countries that are still developing may have lower
            populations and food availability, while countries that are just geographically small might only have smaller populations. Having this variable is important to
            identify which countries may be struggling the most with food; pure food availability would not tell the whole story."),
          
          h3("Interactive Guide:"),
          p("Use the slider to choose one year to look at. The data is ordered so that countries with the least food per person are at the top of the graph, and countries
            with the most food per person are at the bottom. Use the zoom functionality to focus in on specific countries."),
          
          h3("Key Takeaways and Analysis:"),
          p("Observe how a country's amount of food per person changes over time, and be sure to consider all of the important factors. The temperature will change,
            the population will likely increase, and the country may make technological advancements. Compare and contrast the data between different countries. Synthesize
            this data with information from the other tabs for better understanding of the interactions betweeen these variables."),
          plotlyOutput(outputId = "food_histo"),
        ),
      ),
    ),
    
    # Tab for Adaptive Strategies
    tabPanel("Adaptive Strategies",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "temp_country",
            label = "Choose a country to highlight",
            choices = joined_df$Country,
            selected = "",
          ),
          h6("(*Removed country Côte d'Ivoire due to string error)"),
        ),
        
        mainPanel(
          h2("How Countries Have Dealt With the Heat:"),
          p("In this graph, we directly compare temperature anomalies over time with food per person. This will reveal how individual countries have reacted to climate change
            in regards to their food security. Countries are differently equipped to handle the effects of climate change, and that is reflected in how their amounts of food
            per person changes over time in respones to tempertaure anomalies."),
          
          h3("Interactive Guide:"),
          p("Use the drop-down menu to choose a country. The graph will show how that country's food availability changed from 2003-2013. Each data point is also labeled
            with the temperature anomaly for that year."),
          
          h3("Key Takeaways and Analysis:"),
          p("Observe the trends in how food availability changes over time. Identify the countries whose food availabilities are most and least affected by climate change.
          Note that this dataset does not contain every single country. Evaluate how strongly correlated food availability is with temperature anomalies."),
          
          plotlyOutput(outputId = "scat"),
        ),
      ),
    ),
    
    # Summary Takeaways & About Page
    tabPanel("Summary & About",
             h2("Key Takeaways"),
             p("Through these data visualizations, we have seen how climate change, food production, progression of time, and population size interact with each other in
             complex ways. A country may belong to a higher food amount category compared to another country, but less food per person due to differences in population. It is difficult
             to gauge the extent that temperaure affects food availability due to the variety of possible confounding variables."),
             
             h3("Notable Data Points"),
             p("Of the countries in the dataset, the Democratic Republic of the Congo (listed as 'DR Congo') consistently had the lowest amount of food per person, while
               Egypt consistently had the highest amount. It is important to know which countries may be struggling the most with food production."),
             p("The year from our chosen range with the highest temperature anomaly was the most recent year, 2013, with a global temperature increase of 0.77 degrees (Celsius).
               Additionally, all of the anomalies from 2003-2013 are positive. This means that not only has temperature exclusively increased throughout this period, but the amount
               that it increases grows with each year."),
             
             h3("What Now?"),
             p("While climate change and the food supply chain will continue to be problems in the foreseeable future, we now have tools that can inform us as to how to best allocate
               our resources to deal with these issues."),
             
             hr(),
             
             h2("About the Team"),
             p("This analysis was conducted by Thuy Nguyen, Nguyen Le, and Nola Fung."),
             p("Data sources: https://data.nal.usda.gov/dataset/international-food-security-0"),
             p("https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/global/time-series")
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

  
  # Server logic for Supply Chain Resilience
  output$food_histo <- renderPlotly({
    food_per_df <- select(joined_df, Country, kg_food_per_person, Year)
    food_per_df <- unique(food_per_df)

    #Round up kg_food_per_person
    food_per_df$kg_food_per_person <- round(food_per_df$kg_food_per_person, 2)
    #Filter only the selected year
    food_per_df <- filter(food_per_df, Year == input$food_per_year)
    
    
    food_bar <- ggplot(data = food_per_df, aes(x = kg_food_per_person, y = reorder(Country, -kg_food_per_person), text = Country)) +
      geom_bar(stat = "identity") +
      #geom_text(aes(hjust = -0.1, vjust = -0.3)) +
      labs(x = "Food Per Person (kg)", y = "Country", title = "Food Availability Per Person Of Diffrent Countries In The Selected Year") +
      theme(axis.text = element_text(size = 6)) 
    
    return(food_bar)
  })
  
  # Server logic for Adaptive Strategies
  output$scat <- renderPlotly({
    food_temp_df <- select(joined_df, Country, kg_food_per_person, Year, Anomaly)
    food_temp_df <- unique(food_temp_df)
    
    #Round up kg_food_per_person
    food_temp_df$kg_food_per_person <- round(food_temp_df$kg_food_per_person, 2)
    
    #Filter only the selected year
    food_temp_df <- filter(food_temp_df, Country == input$temp_country)
    
    scat_plot <- ggplot(data = food_temp_df, aes(x = Year, y = kg_food_per_person, text = Country)) +
      geom_line(color = "darkgreen") +
      geom_text(aes(label = Anomaly)) +
      labs(x = "Year", y = "Food Per Person (kg)", color = "Selected Country",
           title = "Food Availability Vs Temperature Anomaly Of Different Countries In The Selected Year")
    
    #scat_plot <- ggplotly(scat_plot, tooltip = "text")
    return(scat_plot)
  })
}

# Run the app
shinyApp(ui, server)
