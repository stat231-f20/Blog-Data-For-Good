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
  select(-c(population))

#Variable choices for the map - 
#need to go back and divide by population for proportions and per capita values - also clean up variable names
variable_choices <- as.list(names(cities_long)[11:26])
map_var_choices <- c("Median Age (Male)", "Median Age (Female)", "White Population", "Black Population", 
                  "Foreign Born Population", "Vehicles Available", 
                  "Total Households", "Total Married Households", "Number of Male Bachelors",
                  "Number of Female Bachelors", "Poverty Index", "Median Income", "Hours Worked", 
                  "Houses for Sale", "Median Home Value", "Number of People Who Speak Only English")
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
            
             leafletOutput("map")
             
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
    palette = "Spectral",
    domain = use_data_map()$interest_var)
  })

  #leaflet map
  output$map <- renderLeaflet({
    leaflet(use_data_map()) %>% 
      addTiles() %>%
      setView(-72.5, 42.4, zoom = 3) %>%
      addCircleMarkers(lat= ~latitude
                       , lng= ~longitude
                       , popup= paste0(use_data_map()$name_simp,", ", use_data_map()$state_abbrev, "<br>",
                                       map_var_choices[variable_choices == input$var], ": ", use_data_map()$interest_var)
                       , stroke = FALSE 
                       , radius = 5
                       , fillColor = ~pal()(use_data_map()$interest_var)
                       , fillOpacity = 0.9)
  })
}

# call to shinyApp
shinyApp(ui = ui, server = server)

