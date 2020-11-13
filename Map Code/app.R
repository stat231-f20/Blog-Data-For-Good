library(tidyverse) 
library(datasets)
library(viridis)
library(maps)
library(leaflet)
library(rvest)
library(robotstxt)
library(tidycensus)
library(tidytext)
library(shiny)
library(shinythemes)

path_in <- "/Users/glecates/git/Blog-Data-For-Good/"
city_data <- read_csv(paste0(path_in, "dataset.csv"))


#put data in long form
common_vars <- colnames(city_data[c(1:3, 21:27)])
short_vars <- c("median_age_male", "median_age_female", "population", "white_alone", "black_alone",
                "foreign_born", "vehicles_avail", "total_hh", "married_hh", "male_bachelors", "female_bachelors",
                "poverty", "median_income", "hours_worked", "houses_for_sale", "median_value", "speaks_only_english")

data13 <- city_data%>%
  select(c(common_vars, str_which(colnames(city_data), "2013")))%>%mutate(year = 2013)
colnames(data13)<-c(common_vars, short_vars, "year")

data14 <- city_data%>%
  select(c(common_vars, str_which(colnames(city_data), "2014")))%>%mutate(year = 2014)
colnames(data14)<-c(common_vars, short_vars, "year")

data15 <- city_data%>%
  select(c(common_vars, str_which(colnames(city_data), "2015")))%>%mutate(year = 2015)
colnames(data15)<-c(common_vars, short_vars, "year")

data16 <- city_data%>%
  select(c(common_vars, str_which(colnames(city_data), "2016")))%>%mutate(year = 2016)
colnames(data16)<-c(common_vars, short_vars, "year")

data17 <- city_data%>%
  select(c(common_vars, str_which(colnames(city_data), "2017")))%>% mutate (year = 2017)
colnames(data17)<-c(common_vars, short_vars, "year")

data18 <- city_data%>%
  select(c(common_vars, str_which(colnames(city_data), "2018")))%>%mutate(year = 2018)
colnames(data18)<-c(common_vars, short_vars, "year")

dfs <- lst(data13, data14, data15, data16, data17, data18)

cities_long <- bind_rows(dfs) %>%
  mutate(bachelors = female_bachelors + male_bachelors) %>%
  rename(num_vehicles = vehicles_avail) %>%
  select(NAME, name_simp, state_abbrev, name_classic, state, region, latitude, longitude, 
         population, median_age_male, median_age_female, white_alone, black_alone, foreign_born, 
         total_hh, married_hh, median_income, houses_for_sale, median_value, speaks_only_english, 
         num_vehicles, poverty, bachelors, year, median_value) %>%
  
  #get standardized/per capita variables
  mutate(pct_white = white_alone/population,
         pct_black = black_alone/population,
         pct_foreign_born = foreign_born/population,
         pct_married_hh = married_hh/total_hh,
         per_capita_for_sale = houses_for_sale/population,
         pct_speak_only_english = speaks_only_english/population,
         vehicles_per_hh = num_vehicles/total_hh,
         pct_poverty = poverty/population,
         pct_bachelors = bachelors/population) %>%
  
  #selecting only standardized values
  select(NAME, 
         name_simp, 
         state_abbrev, 
         name_classic, 
         state, 
         region, 
         latitude, 
         longitude, 
         median_income,
         median_value,
         median_age_male, 
         median_age_female, 
         population, 
         pct_white, 
         pct_black, 
         pct_foreign_born, 
         pct_married_hh, 
         per_capita_for_sale,
         pct_speak_only_english, 
         vehicles_per_hh, 
         pct_poverty, 
         pct_bachelors,
         year)

cities_long$region = ifelse(cities_long$state_abbrev == "PR", "South", cities_long$region)

#Shiny app variable choices
variable_choices <- as.list(names(cities_long)[9:22])
map_var_choices <- c("Median Income",
                     "Median Home Value",
                     "Median Age (Male)", 
                     "Median Age (Female)",
                     "Population",
                     "Percent White Population",
                     "Percent Black Population",
                     "Percent Foreign Born Population",
                     "Percent Married Couple Households (of total households)",
                     "Houses for Sale (Per Capita)",
                     "Percent Only English-Speaking Population",
                     "Vehicles per Household",
                     "Percent of Population Below Poverty Line",
                     "Percent Bachelors (Male and Female)")

names(variable_choices) <- map_var_choices

map_year_choices <- as.numeric(c("2013", "2014", "2015", "2016", "2017", "2018"))

#ui
ui <- fluidPage(
  
    tabPanel(title = "Map of US Cities",
             
             selectInput(inputId = "var"
                         , label = "Choose a variable of interest:"
                         , choices = variable_choices),
             selectInput(inputId = "year"
                         , label = "Choose a year of interest:"
                         , choices = map_year_choices),
            
             leafletOutput("map", height = 700)
    )
)

server <- function(input,output){
  
  #map data set
  use_data_map <- reactive({
    data <- cities_long %>%
      filter(year == input$year) %>%
      rename('interest_var' = input$var)
  })
  
  pal <- reactive ({
    mypal <- colorNumeric(
    palette = "OrRd",
    domain = use_data_map()$interest_var)
  })

  #leaflet map
  output$map <- renderLeaflet({
    leaflet(use_data_map()) %>% 
      addTiles() %>%
      setView(-97.5, 37.4, zoom = 3.5) %>%
      addCircleMarkers(lat= ~latitude
                       , lng= ~longitude
                       , popup= paste0(use_data_map()$name_simp,", ", use_data_map()$state_abbrev, "<br>",
                                       map_var_choices[variable_choices == input$var], ": ", use_data_map()$interest_var)
                       , stroke = FALSE 
                       , radius = 7
                       , fillColor = ~pal()(use_data_map()$interest_var)
                       , fillOpacity = 1.0) %>%
      
      addLegend(pal = pal(), 
                values = use_data_map()$interest_var, 
                position = "bottomright", 
                title = paste0(map_var_choices[variable_choices == input$var]),
                opacity = 1.0)
  })
}

# call to shinyApp
shinyApp(ui = ui, server = server)

