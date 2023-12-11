library(shiny)
library(dplyr)
library(ggplot2)
library(fmsb)
source("CLEANPROJECTDATASET.R")

proj_df <- read.csv("projectdataframe.csv")

about_view <- fluidPage(
  h1("Intro Page"),
  p("Stuff goes here"),
  p("yayaya")
)

analysis_view <- fluidPage(
  titlePanel("Disorder analysis"),
  sidebarLayout(
    sidebarPanel(
      
      selectInput(
        inputId = "country",
        label = "Select a country",
        choices = proj_df$Entity
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
      )
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput(outputId = "graph"))
      )
    )
  )
  
)

ui <- navbarPage(
  "Final Project",
  tabPanel("Introduction", about_view),
  tabPanel("Disorders", analysis_view),
  tabPanel("Depression", depression_view)
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
}


#what actually makes the shiny app
shinyApp(ui = ui, server = server)
