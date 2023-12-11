library(shiny)
library(dplyr)
library(ggplot2)
library(fmsb)
source("CLEANPROJECTDATASET.R")

proj_df <- read.csv("projectdataframe.csv")

about_view <- fluidPage(
  about_view <- fluidPage(
    titlePanel("Understanding Global Mental Health Trends"),
    sidebarLayout(
      sidebarPanel(
        h3("About the Project"),
        p("In this project, we delve into the complex world of mental health, focusing on the prevalence and severity of depression disorders across the globe. Our analysis aims to shed light on patterns and disparities in mental health, emphasizing the importance of informed and empathetic understanding of these issues."),
        p("Why is this important? Mental health is a crucial aspect of overall well-being, yet it often remains overlooked or misunderstood. By analyzing comprehensive datasets, we hope to contribute to a more nuanced conversation about mental health and its impact on individuals and societies.")
      ),
      mainPanel(
        h4("Dataset Overview"),
        p("This analysis utilizes several datasets, including the Mental Health Depression Disorder Dataset, along with data on the prevalence of depression in various demographics."),
        p("Key aspects of our analysis include examining global trends in mental health disorders, gender-specific prevalence rates, and exploring the socioeconomic factors that influence mental health outcomes."),
        h4("Why Should You Care?"),
        p("The insights gained from this analysis are not just numbers and graphs; they represent real people and real challenges. Understanding these trends is vital for policymakers, healthcare providers, and communities to develop effective strategies for mental health care and support."),
        h4("Additional Resources"),
        p(a("Learn more about mental health", href = "https://www.who.int/health-topics/mental-health#tab=tab_1", target = "_blank")),
        p(a("World Health Organization: Mental Health", href = "https://www.who.int/news-room/fact-sheets/detail/mental-health-strengthening-our-response", target = "_blank"))
      )
    )
  )
)

total_view <- fluidPage(
  titlePanel("Disorder Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "country",
        label = "Select a country",
        choices = unique(proj_df$Entity)
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table", tableOutput(outputId = "table")),
        tabPanel("Plot", plotOutput(outputId = "radar"))
        
      )
    )
  )
)

depression_view <- fluidPage(
  titlePanel("Depression analysis"),
  sidebarLayout(
    sidebarPanel(
      
      selectInput(
        inputId = "Entity",
        label = "Select a country",
        choices = proj_df$Entity
      ),
      htmlOutput(outputId = "total"),
      br()
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput(outputId = "graph"))
      )
    )
  )
)

analysis_view <- fluidPage(
  titlePanel("Mental Health Trends"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select a country", choices = unique(proj_df$Entity)),
      selectInput("disorder", "Select a disorder", choices = c("Depression", "Anxiety"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput(outputId = "trends_plot"))
      )
    )
  )
)


ui <- navbarPage(
  "Mental Health Data Exploration",
  tabPanel("Introduction", about_view),
  tabPanel("Disorders by Country", total_view),
  tabPanel("Mental Health Trends", analysis_view),
  tabPanel("Depression by gender", depression_view)
)

server <- function(input, output) {
  make_radar_tb <- function(name) {
    data_pt <- subset(proj_df, Entity == name)
    data_pt <- select(data_pt, -c(Entity, index, depression.severity, Year, Code, Continent))
    max_pt <- summarise_all(proj_df, max)
    max_pt <- select(max_pt, -c(Entity, index, depression.severity, Year, Code, Continent))
    
    min_pt <- summarise_all(proj_df, min)
    min_pt <- select(min_pt, -c(Entity, index, depression.severity, Year, Code, Continent))
    
    do.call("rbind", list(max_pt, min_pt, data_pt))
  }
  
  create_trends_plot <- function(df, country, disorder) {
    plot_data <- df %>%
      filter(Entity == country) %>%
      select(Year, !!as.name(disorder))
    
    plot <- ggplot(plot_data, aes(x = Year, y = !!sym(disorder))) +
      geom_line() +
      labs(title = paste("Trends in", disorder, "Prevalence for", country),
           x = "Year",
           y = "Prevalence (%)") +
      theme_minimal()
    
    return(plot)
  }
  
  output$trends_plot <- renderPlot({
    country <- input$country
    disorder <- switch(
      input$disorder,
      "Depression" = "Depression_Prevalence",
      "Anxiety" = "Anxiety_Prevalence",
      "Depression_Prevalence" = "Depression_Prevalence"  # Default to Depression if not selected
    )
    
    plot <- create_trends_plot(proj_df, country, disorder)
    
    # Alternatively, you can use 'return' (though 'print' is not always necessary)
    return(plot)
  })
  
  
  
  plot_depression_gender <- function(name) {
    data_pt <- subset(proj_df, Entity == name)
    filtered_data <- select(data_pt, Year, 
                            "Prevalence...Depressive.disorders...Sex..Male...Age..Age.standardized..Percent.",
                            "Prevalence...Depressive.disorders...Sex..Female...Age..Age.standardized..Percent.")
    
    plot <- ggplot(filtered_data, aes(x = Year)) +
      geom_line(aes(y = `Prevalence...Depressive.disorders...Sex..Male...Age..Age.standardized..Percent.`, color = "Male")) +
      geom_line(aes(y = `Prevalence...Depressive.disorders...Sex..Female...Age..Age.standardized..Percent.`, color = "Female")) +
      labs(title = paste("Depression Prevalence in", name, ": Male vs Female"),
           x = "Year",
           y = "Prevalence (%)",
           color = "Gender") +
      theme_minimal()
    
    return(plot)
  }
  
  output$total <- renderUI({
    get_average_severity <- function(df, country) {
      country_average <- mean(df$depression.population[df$Entity == country], na.rm = TRUE)
      total_text <- paste("Average Depression population", country, ":", round(country_average))
      return(total_text)
    }
    
    country <- input$Entity
    total_text <- get_average_severity(proj_df, country)
    total_html <- HTML(total_text)
    
    return(total_html)
  })
  
  output$table <- renderTable({
    return(make_radar_tb(input$country))
  })
  
  output$radar <- renderPlot({
    tb <- make_radar_tb(input$country)
    radarchart(tb)
  })
  
  output$graph <- renderPlot({
    plot_depression_gender(input$Entity)
  })
}

#what actually makes the shiny app
shinyApp(ui = ui, server = server)
