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

cities_long <- bind_rows(dfs)

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


library(shinythemes)
library(tidyverse)
library(rvest)
library(robotstxt)
library(tidycensus)
library(tidytext)
library(shiny)

path_in <- "/Users/steedmanjenkins/git/Shiny-Data-for-Good/"

#grace dataset (for scatterplot)
scat_city_data <- read_csv(paste0(path_in, "wrangled_dataset.csv")) %>%
  select(names_clean, state, population, median_income, median_value, p_fb, 
         p_only_english, vehicles_per_capita, no_int_per_capita, 
         for_sale_per_capita, from_abroad_per_capita, poverty_rate, college_per_capita,
         state.y, division, region) %>%
  filter(region %in% c("Northeast", "Midwest", "South", "West")) %>%
  rename('Region' = 'region', 'Population' = 'population')



#grace choices (for scatterplot)
scat_x_choices <- as.list(names(scat_city_data)[6:13])
scat_x_names <- c("Percent Foreign Born", "Percent Only English-Speaking", "Vehicles per Capita", 
                  "Percent Without Internet Access", "Houses for Sale per Capita", 
                  "Percent Moved from Abroad", "Poverty Rate", "Percent with College Degree")
names(scat_x_choices) <- scat_x_names

scat_y_choices <- as.list(names(scat_city_data)[4:5])
scat_y_names <- c("Median Income ($)", "Median Home Value ($)")
names(scat_y_choices) <- scat_y_names

scat_region_choices <- (scat_city_data %>%
                          count(Region))$Region

scat_city_choices <- as.list(c("None", scat_city_data$names_clean))
names(scat_city_choices) <- c("None", scat_city_data$names_clean)


#ui
ui <- fluidPage(
  
  h1("Data Visualizations for the top 100 most populated US Metro Areas in 2018"),
  h5("Grace, Mike, Rodrigo and Steedman"),
  
  navlistPanel(
    
    #grace (scatterplot ui code)
    tabPanel(title = "Comparison of Variables with a Scatterplot",
             
             selectInput(inputId = "x"
                         , label = "Choose a predictor variable of interest:"
                         , choices = scat_x_choices),
             selectInput(inputId = "y"
                         , label = "Choose a response variable of interest:"
                         , choices = scat_y_choices),
             selectInput(inputId = "city"
                         , label = "Identitfy a city in the scatterplot:"
                         , choices = scat_city_choices),
             checkboxGroupInput(inputId = "location"
                                , label = "Choose a region of the U.S:"
                                , choices = scat_region_choices
                                , selected = scat_region_choices
                                , inline = TRUE),
             
             plotOutput(outputId = "scatter")),

    
    

  )
)

server <- function(input,output){
  
  #grace scatter plot data
  use_data_grace <- reactive({
    data <- scat_city_data %>%
      filter(Region%in%input$location)
  })
  
  
  #grace scatterplot
  output$scatter <- renderPlot({
    ggplot(data = use_data_grace(), aes_string(x = input$x, y = input$y)) +
      geom_point(aes(color = Region, size = Population)) +
      labs(x = names(scat_x_choices)[scat_x_choices == input$x]
           ,y = names(scat_y_choices)[scat_y_choices == input$y]) +
      geom_label(data = filter(scat_city_data, names_clean == input$city), aes(label = names_clean))
  })
  
  
  
 
  
}

# call to shinyApp
shinyApp(ui = ui, server = server)


