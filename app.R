library(shiny)
library(leaflet)
library(tidyverse)

source("R/sampling_functions.R")

# Read in the localities data
localities <- read_csv("data/localities.csv")
pokemon <- read_csv("data/pokemon.csv") %>%
  select(name, pokedex_number, generation, type1, type2, is_legendary, percentage_male, height_m, weight_kg, hp, attack, defense, speed, sp_attack, sp_defense, base_total) %>%
  mutate(
    # name = case_when(name == "Mr. Mime" ~ "Mr Mime",
    #                  name == "Mime Jr." ~ "Mime Jr",
    #                  name == "Nidoran♀" ~ "Nidoran F",
    #                  name == "Nidoran♂" ~ "Nidoran M",
    #                  name == "Type: Null" ~ "Type Null",
    #                  T ~ name),
    generation = case_when(generation == 1 ~ "I",
                           generation == 2 ~ "II",
                           generation == 3 ~ "III",
                           generation == 4 ~ "IV",
                           generation == 5 ~ "V",
                           generation == 6 ~ "VI",
                           generation == 7 ~ "VII"))

# Define the UI for the app
ui <- fluidPage(
  h3("Select a site to visit:"),
  leafletOutput("map"),
  uiOutput("sample_page")
)

# Define the server logic for the app
server <- function(input, output, session) {
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lat = -27.3,
              lng = 133.1, 
              zoom = 3) %>%
      addTiles() %>%
      addMarkers(data = localities, 
                 lat = ~latitude, 
                 lng = ~longitude,
                 layerId = ~id,
                 group = "Localities")
  })
  
  # When a marker is clicked, open a new page displaying the locality's information
  observeEvent(input$map_marker_click, {
    selected_locality <- localities[localities$id == input$map_marker_click$id,]
    
    # Save the values from the table in a separate object
    values_table <- as.numeric(selected_locality[,c("temp", "rainfall", "humid", "wind")])
    
    # Add random variability to the sampled values
    temp <- values_table[1] + rnorm(1, mean = 0, sd = 3) # Temperature
    rainfall <- max(0, values_table[2] + rnorm(1, mean = 0, sd = 100)) # Rainfall
    humid <- min(max(0, values_table[3] + rnorm(1, mean = 0, sd = 20)), 100) # Humidity
    wind <- max(0, values_table[4] + rnorm(1, mean = 0, sd = 10)) # Wind
    
    # Sample Pokemon
    pokemon_weights <- pokemon %>%
      gather(position, typing, c(type1, type2)) %>%
      drop_na() %>%
      mutate(sampling_weight = case_when(typing == "fire" ~ func_fire(temp),
                                         typing == "ice" ~ 1- func_fire(temp),
                                         typing == "water" ~ func_water(rainfall),
                                         typing == "ground" ~ 1 - func_water(rainfall),
                                         typing == "grass" ~ func_grass(humid),
                                         typing == "electric" ~ 1 - func_grass(humid),
                                         typing == "flying" ~ func_flying(wind),
                                         T ~ 0.5)) %>%
      select(-c(position, typing)) %>%
      group_by(name, pokedex_number, generation, is_legendary, percentage_male, height_m, weight_kg, hp, attack, defense, speed, sp_attack, sp_defense, base_total) %>%
      summarise(sampling_weight = mean(sampling_weight)) %>%
      left_join(pokemon) %>%
      arrange(pokedex_number)
    
    sampled_pokemon <- sample(1:nrow(pokemon_weights), 6, prob = pokemon_weights$sampling_weight)
    
    sampled_pokemon <- pokemon_weights[sampled_pokemon, ]
    
    output$sample_page <- renderUI({
      fluidPage(
        h3(selected_locality$name),
        fluidRow(paste0("Temperature: ", round(temp, 2),
                        "°C, Rainfall: ", round(rainfall, 2),
                        " mm, Relative Humidity: ", round(humid, 2),
                        "%, Wind Speed: ", round(wind, 2),
                        " km/h")),
        br(),
        p("You encountered the following Pokemon!"),
        fluidRow(column(2,
                        strong(sampled_pokemon[1,]$name),
                        br(),
                        paste("Generation:", sampled_pokemon[1,]$generation),
                        br(),
                        paste("Type:", paste(na.omit(c(sampled_pokemon[1,]$type1, sampled_pokemon[1,]$type2)), collapse = ", ")),
                        br(),
                        paste("Sex:", sample(c("Male", "Female"),
                                             size = 1,
                                             prob = c(sampled_pokemon[1,]$percentage_male/100,
                                                      1 - sampled_pokemon[1,]$percentage_male/100))),
                        br(),
                        paste("Height:", sampled_pokemon[1,]$height_m, "m"),
                        br(),
                        paste("Weight:", sampled_pokemon[1,]$weight_kg, "kg"),
                        br(),
                        paste("HP:", sampled_pokemon[1,]$hp),
                        br(),
                        paste("Attack:", sampled_pokemon[1,]$attack),
                        br(),
                        paste("Defense:", sampled_pokemon[1,]$defense),
                        br(),
                        paste("Sp. Attack:", sampled_pokemon[1,]$sp_attack),
                        br(),
                        paste("Sp. Defense:", sampled_pokemon[1,]$sp_defense),
                        br(),
                        paste("Speed:", sampled_pokemon[1,]$speed)),
                 column(2,
                        strong(sampled_pokemon[2,]$name),
                        br(),
                        paste("Generation:", sampled_pokemon[2,]$generation),
                        br(),
                        paste("Type:", paste(na.omit(c(sampled_pokemon[2,]$type1, sampled_pokemon[2,]$type2)), collapse = ", ")),
                        br(),
                        paste("Sex:", sample(c("Male", "Female"),
                                             size = 1,
                                             prob = c(sampled_pokemon[2,]$percentage_male/100,
                                                      1 - sampled_pokemon[2,]$percentage_male/100))),
                        br(),
                        paste("Height:", sampled_pokemon[2,]$height_m, "m"),
                        br(),
                        paste("Weight:", sampled_pokemon[2,]$weight_kg, "kg"),
                        br(),
                        paste("HP:", sampled_pokemon[2,]$hp),
                        br(),
                        paste("Attack:", sampled_pokemon[2,]$attack),
                        br(),
                        paste("Defense:", sampled_pokemon[2,]$defense),
                        br(),
                        paste("Sp. Attack:", sampled_pokemon[2,]$sp_attack),
                        br(),
                        paste("Sp. Defense:", sampled_pokemon[2,]$sp_defense),
                        br(),
                        paste("Speed:", sampled_pokemon[2,]$speed)),
                 column(2,
                        strong(sampled_pokemon[3,]$name),
                        br(),
                        paste("Generation:", sampled_pokemon[3,]$generation),
                        br(),
                        paste("Type:", paste(na.omit(c(sampled_pokemon[3,]$type1, sampled_pokemon[3,]$type2)), collapse = ", ")),
                        br(),
                        paste("Sex:", sample(c("Male", "Female"),
                                             size = 1,
                                             prob = c(sampled_pokemon[3,]$percentage_male/100,
                                                      1 - sampled_pokemon[3,]$percentage_male/100))),
                        br(),
                        paste("Height:", sampled_pokemon[3,]$height_m, "m"),
                        br(),
                        paste("Weight:", sampled_pokemon[3,]$weight_kg, "kg"),
                        br(),
                        paste("HP:", sampled_pokemon[3,]$hp),
                        br(),
                        paste("Attack:", sampled_pokemon[3,]$attack),
                        br(),
                        paste("Defense:", sampled_pokemon[3,]$defense),
                        br(),
                        paste("Sp. Attack:", sampled_pokemon[3,]$sp_attack),
                        br(),
                        paste("Sp. Defense:", sampled_pokemon[3,]$sp_defense),
                        br(),
                        paste("Speed:", sampled_pokemon[3,]$speed)),
                 column(2,
                        strong(sampled_pokemon[4,]$name),
                        br(),
                        paste("Generation:", sampled_pokemon[4,]$generation),
                        br(),
                        paste("Type:", paste(na.omit(c(sampled_pokemon[4,]$type1, sampled_pokemon[4,]$type2)), collapse = ", ")),
                        br(),
                        paste("Sex:", sample(c("Male", "Female"),
                                             size = 1,
                                             prob = c(sampled_pokemon[4,]$percentage_male/100,
                                                      1 - sampled_pokemon[4,]$percentage_male/100))),
                        br(),
                        paste("Height:", sampled_pokemon[4,]$height_m, "m"),
                        br(),
                        paste("Weight:", sampled_pokemon[4,]$weight_kg, "kg"),
                        br(),
                        paste("HP:", sampled_pokemon[4,]$hp),
                        br(),
                        paste("Attack:", sampled_pokemon[4,]$attack),
                        br(),
                        paste("Defense:", sampled_pokemon[4,]$defense),
                        br(),
                        paste("Sp. Attack:", sampled_pokemon[4,]$sp_attack),
                        br(),
                        paste("Sp. Defense:", sampled_pokemon[4,]$sp_defense),
                        br(),
                        paste("Speed:", sampled_pokemon[4,]$speed)),
                 column(2,
                        strong(sampled_pokemon[5,]$name),
                        br(),
                        paste("Generation:", sampled_pokemon[5,]$generation),
                        br(),
                        paste("Type:", paste(na.omit(c(sampled_pokemon[5,]$type1, sampled_pokemon[5,]$type2)), collapse = ", ")),
                        br(),
                        paste("Sex:", sample(c("Male", "Female"),
                                             size = 1,
                                             prob = c(sampled_pokemon[5,]$percentage_male/100,
                                                      1 - sampled_pokemon[5,]$percentage_male/100))),
                        br(),
                        paste("Height:", sampled_pokemon[5,]$height_m, "m"),
                        br(),
                        paste("Weight:", sampled_pokemon[5,]$weight_kg, "kg"),
                        br(),
                        paste("HP:", sampled_pokemon[5,]$hp),
                        br(),
                        paste("Attack:", sampled_pokemon[5,]$attack),
                        br(),
                        paste("Defense:", sampled_pokemon[5,]$defense),
                        br(),
                        paste("Sp. Attack:", sampled_pokemon[5,]$sp_attack),
                        br(),
                        paste("Sp. Defense:", sampled_pokemon[5,]$sp_defense),
                        br(),
                        paste("Speed:", sampled_pokemon[5,]$speed)),
                 column(2,
                        strong(sampled_pokemon[6,]$name),
                        br(),
                        paste("Generation:", sampled_pokemon[6,]$generation),
                        br(),
                        paste("Type:", paste(na.omit(c(sampled_pokemon[6,]$type1, sampled_pokemon[6,]$type2)), collapse = ", ")),
                        br(),
                        paste("Sex:", sample(c("Male", "Female"),
                                             size = 1,
                                             prob = c(sampled_pokemon[6,]$percentage_male/100,
                                                      1 - sampled_pokemon[6,]$percentage_male/100))),
                        br(),
                        paste("Height:", sampled_pokemon[6,]$height_m, "m"),
                        br(),
                        paste("Weight:", sampled_pokemon[6,]$weight_kg, "kg"),
                        br(),
                        paste("HP:", sampled_pokemon[6,]$hp),
                        br(),
                        paste("Attack:", sampled_pokemon[6,]$attack),
                        br(),
                        paste("Defense:", sampled_pokemon[6,]$defense),
                        br(),
                        paste("Sp. Attack:", sampled_pokemon[6,]$sp_attack),
                        br(),
                        paste("Sp. Defense:", sampled_pokemon[6,]$sp_defense),
                        br(),
                        paste("Speed:", sampled_pokemon[6,]$speed))
        )
      )
    })
    
  })
}

shinyApp(ui, server)
