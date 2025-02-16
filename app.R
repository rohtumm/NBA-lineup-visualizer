library(shiny)
library(tidyverse)
library(ggplot2)
library(scales)
library(rsconnect)

# Load data
df <- read.csv("lineups_four_factors_2_16_2025.csv")
data <- df %>%
  rename(
    `Offense: Pts/Possession` = "OFFENSE..Pts.Poss",
    `Offensive eFG%` = "OFFENSE..eFG.",
    `Offensive TOV%` = "OFFENSE..TOV.",
    `Offensive ORB%` = "OFFENSE..ORB.",
    `Offensive FT Rate` = "OFFENSE..FT.Rate",
    `Defense: Pts/Possession` = "DEFENSE..Pts.Poss",
    `Defensive eFG%` = "DEFENSE..eFG.",
    `Defensive TOV%` = "DEFENSE..TOV.",
    `Defensive ORB%` = "DEFENSE..ORB.",
    `Defensive FT Rate` = "DEFENSE..FT.Rate",
    `Points/Poss Differential` = "Diff"
  ) %>%
  mutate(across(c(`Offense: Pts/Possession`, `Offensive eFG%`, `Offensive TOV%`, `Offensive ORB%`, `Offensive FT Rate`, `Defense: Pts/Possession`,
                  `Defensive eFG%`, `Defensive TOV%`, `Defensive ORB%`, `Defensive FT Rate`,
                  `Points/Poss Differential`), ~ as.numeric(str_remove(.x, "%")), .names = "{.col}"))

# Remove rows with NA values after coercion
df <- data[!is.na(data$Team) & data$Team != 'League Averages' & complete.cases(data), ]

# Filter columns to include only percentage and rate stats
stat_columns <- c("Offense: Pts/Possession", "Offensive eFG%", "Offensive TOV%", "Offensive ORB%", "Offensive FT Rate", "Defense: Pts/Possession",
                  "Defensive eFG%", "Defensive TOV%", "Defensive ORB%", "Defensive FT Rate", 
                  "Points/Poss Differential")

ui <- fluidPage(
  titlePanel("NBA Lineup Rankings"),
  sidebarLayout(
    sidebarPanel(
      selectInput("stat", "Select a Stat to Visualize:", choices = stat_columns),
      textOutput("description"),
      numericInput("min_poss", "Minimum Possessions:", value = 100, min = 0)
    ),
    mainPanel(
      plotOutput("boxplot")
    )
  ),
  hr(),
  p("This NBA Lineup Rankings app allows you to explore the effectiveness of various NBA teams' lineups based on different metrics. Select a stat and set a minimum posessions threshold to visualize performances across all of a team's different lineups played this season. Each individual boxplot displays data from all lineups that specific team played for the minimum possessions threshold value. Data is sourced from official NBA statistics.")
)

server <- function(input, output) {
  descriptions <- list(
    `Offense: Pts/Possession` = "The total number of points scored by a lineup divided by the number of posessions they have played together.",
    `Offensive eFG%` = "Effective Field Goal Percentage measures shooting efficiency by weighting 3-pointers more.",
    `Offensive TOV%` = "Turnover Percentage is the ratio of turnovers per offensive play.",
    `Offensive ORB%` = "Offensive Rebound Percentage is the rate of getting offensive rebounds.",
    `Offensive FT Rate` = "Free Throw Rate measures how often a team gets to the line relative to field goals.",
    `Defense: Pts/Possession` = "The total number of points allowed by a lineup divided by the number of posessions they have played together.",
    `Defensive eFG%` = "Defensive Effective Field Goal Percentage measures opponent shooting efficiency.",
    `Defensive TOV%` = "Defensive Turnover Percentage is the rate of forcing opponent turnovers.",
    `Defensive ORB%` = "Defensive Rebound Percentage is the rate of grabbing defensive boards.",
    `Defensive FT Rate` = "Defensive Free Throw Rate measures opponent trips to the line.",
    `Points/Poss Differential` = "Points per Possession Differential is simply Offense Pts/Pos - Defense Pts/Pos."
  )
  
  output$description <- renderText({
    descriptions[[input$stat]]
  })
  filtered_data <- reactive({
    df %>% filter(Poss >= input$min_poss)
  })
  
  output$boxplot <- renderPlot({
    req(input$stat)
    ggplot(filtered_data(), aes(x = reorder(Team, filtered_data()[[input$stat]]), y = filtered_data()[[input$stat]], fill = Team)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Team Lineups Effectiveness in Games", x = "Lineup", y = input$stat) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
}

shinyApp(ui = ui, server = server)
