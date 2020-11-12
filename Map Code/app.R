library(tidyverse) 
library(datasets)
library(viridis)
library(maps)
library(leaflet)
library(shinythemes)
library(rvest)
library(robotstxt)
library(tidycensus)
library(tidytext)
library(shiny)

path_in <- "/Users/glecates/git/Blog-Data-For-Good"
city_data <- read_csv(paste0(path_in, "/dataset.csv"))

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

mypal <- colorNumeric(
  palette = "Spectral",
  domain = city_data$y2013_population)

leaflet(data = city_data) %>% 
  addTiles() %>%
  setView(-72.5, 42.4, zoom = 3) %>%
  addCircleMarkers(lat=city_data$latitude
                   , lng=city_data$longitude
                   , fillColor = ~mypal(y2013_population)
                   , color = "#b2aeae"
                   , popup= paste0(city_data$name_simp,", ", city_data$state_abbrev,": ",city_data$y2013_population)
                   , stroke = FALSE
                   , radius = 5
                   , fillOpacity = 0.9)


#Variable choices for the map - 
#need to go back and divide by population for proportions and per capita values - also clean up variable names
variable_choices <- as.list(cities_long)[11:27]
map_var_choices <- c("Median Age (Male)", "Median Age (Female)", "White Population", "Black Population", 
                  "Foreign Born Population", "Vehicles Available", 
                  "Total Households", "Total Married Households", "Number of Male Bachelors",
                  "Number of Female Bachelors", "Poverty Index", "Median Income", "Hours Worked", 
                  "Houses for Sale", "Median Home Value", "Number of People Who Speak Only English")
names(variable_choices) <- map_var_choices

map_year_choices <- c("2013", "2014", "2015", "2016", "2017", "2018")


#ui
ui <- fluidPage(
  
  h1("Data Visualizations for the top 100 most populated US Metro Areas in 2018"),
  h5("Grace, Mike, Rodrigo and Steedman"),
  
    #grace (scatterplot ui code)
    tabPanel(title = "Map of US Cities",
             
             selectInput(inputId = "vars"
                         , label = "Choose a variable"
                         , choices = map_var_choices),
             selectInput(inputId = "year"
                         , label = "Choose a year of interest"
                         , choices = map_year_choices),
            
             plotOutput(outputId = "map"))

)

server <- function(input,output){
  
  #grace scatter plot data
  use_data_map <- reactive({
    data <- city_long%>%
      filter(year%in%input$year)
  })
  
  use_data_color <- reactive({
    mypal <- colorNumeric(
      palette = "Spectral",
      domain = city_long$var)
  })
  
  #leaflet map  
  
  output$map <- renderPlot({
    leaflet(data = use_data_map()) %>% 
      addTiles() %>%
      setView(-72.5, 42.4, zoom = 3) %>%
      addCircleMarkers(lat=city_data$latitude
                       , lng=city_data$longitude
                       , fillColor = ~mypal(y2013_population)
                       , color = "#b2aeae"
                       , popup= paste0(city_long$name_simp,", ", city_long$state_abbrev,": ",city_long$var)
                       , stroke = FALSE
                       , radius = 5
                       , fillOpacity = 0.9)
  })
}

# call to shinyApp
shinyApp(ui = ui, server = server)


