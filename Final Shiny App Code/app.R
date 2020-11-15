library(tidyverse) 
library(datasets)
library(viridis)
library(maps)
library(leaflet)
library(shiny)
library(shinyWidgets)

#path_in <- "/Users/glecates/git/Blog-Data-For-Good/"
#path_in <- "/Users/CookieCream45/Desktop/STAT-231/Blog-Data-For-Good/"
path_in <- "/Users/steedmanjenkins/git/Blog-Data-For-Good/"

##model data
cities <- read_csv(paste0(path_in, "dataset.csv"))%>%
  mutate(pop_change = y2018_population - y2013_population,
         pct_pop_change = pop_change/y2013_population,
         #get averages across all years for predictor variable values
         population = (y2013_population + y2014_population + y2015_population + 
                         y2016_population + y2017_population + y2018_population)/6,
         median_age_male = (y2013_median_age_male + y2014_median_age_male + y2015_median_age_male +
                              y2016_median_age_male + y2017_median_age_male + y2018_median_age_male)/6,
         median_age_female = (y2013_median_age_female + y2014_median_age_female +  y2015_median_age_female +
                                y2016_median_age_female +y2017_median_age_female + y2018_median_age_female)/6,
         white_alone = (y2013_white_alone + y2014_white_alone + y2015_white_alone + 
                          y2016_white_alone + y2017_white_alone + y2018_white_alone)/6,
         black_alone = (y2013_black_alone + y2014_black_alone + y2015_black_alone + 
                          y2016_black_alone + y2017_black_alone + y2017_black_alone)/6,
         foreign_born = (y2013_f_born + y2014_f_born + y2015_f_born + 
                           y2016_f_born + y2017_f_born + y2018_f_born)/6,
         total_hh = (y2013_total_hh + y2014_total_hh + y2015_total_hh + 
                       y2016_total_hh + y2017_total_hh + y2018_total_hh)/6,
         married_hh = (y2013_married_hh + y2014_total_hh + y2015_total_hh + 
                         y2016_total_hh + y2017_total_hh + y2018_total_hh)/6,
         median_income = (y2013_median_income + y2014_median_income + y2015_median_income + 
                            y2016_median_income + y2017_median_income + y2018_median_income)/6,
         houses_for_sale = (y2013_houses_for_sale + y2014_houses_for_sale + y2015_houses_for_sale + 
                              y2016_houses_for_sale + y2017_houses_for_sale + y2018_houses_for_sale)/6,
         median_value = (y2013_median_value + y2014_median_value + y2015_median_value + 
                           y2016_median_value + y2017_median_value + y2018_median_value)/6,
         only_english = (y2013_speaks_only_english + y2014_speaks_only_english + y2015_speaks_only_english + 
                           y2016_speaks_only_english + y2017_speaks_only_english + y2018_speaks_only_english)/6,
         num_vehicles = (y2013_num_vehicles_avail + y2014_num_vehicles_avail + y2015_num_vehicles_avail + 
                           y2016_num_vehicles_avail + y2017_num_vehicles_avail + y2018_num_vehicles_avail)/6,
         poverty = (y2013_poverty + y2014_poverty + y2015_poverty + 
                      y2016_poverty + y2017_poverty + y2018_poverty)/6,
         female_bachelors = (y2013_female_bachelors + y2014_female_bachelors + y2015_female_bachelors + 
                               y2016_female_bachelors + y2017_female_bachelors + y2018_female_bachelors)/6,
         male_bachelors = (y2013_male_bachelors + y2014_male_bachelors + y2015_male_bachelors + 
                             y2016_male_bachelors + y2017_male_bachelors + y2018_male_bachelors)/6,
         bachelors = female_bachelors + male_bachelors
  )%>%
  #don't need individual year data anymore
  select(NAME, name_simp, state_abbrev, state, region, latitude, longitude, pct_pop_change, pop_change, population, median_age_male, 
         median_age_female,white_alone, black_alone, foreign_born, total_hh, married_hh, 
         median_income, houses_for_sale, median_value, only_english, num_vehicles, poverty, bachelors)%>%
  #get standardized/per capita variables
  mutate(pct_white = white_alone/population,
         pct_black = black_alone/population,
         pct_foreign_born = foreign_born/population,
         pct_married_hh = married_hh/total_hh,
         per_capita_for_sale = houses_for_sale/population,
         pct_only_english = only_english/population,
         vehicles_per_hh = num_vehicles/total_hh,
         pct_poverty = poverty/population,
         pct_bachelors = bachelors/population) %>%
  select(NAME, name_simp, state_abbrev, state, region, latitude, longitude, pct_pop_change, pop_change, population, median_age_male, 
         median_age_female,  median_income, median_value, pct_white, pct_black,pct_foreign_born,pct_married_hh,
         per_capita_for_sale, pct_only_english, vehicles_per_hh, pct_poverty, pct_bachelors)

cities$region = ifelse(cities$state_abbrev == "PR", "South", cities$region)


##map data
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


#model choices
scat_x_choices <- as.list(names(cities)[c(11:23)])
scat_x_names <- c("Median Age Male (Years)", 
                  "Median Age Female (Years)", 
                  "Median Income ($)", 
                  "Median Home Value ($)", 
                  "Percent White Population", 
                  "Percent Black Population", 
                  "Percent Population Foreign Born", 
                  "Percent Married Couple Households", 
                  "Houses for Sale (Per Capita)", 
                  "Percent Population Only English-Speaking", 
                  "Vehicles per Household",
                  "Percent Population Below Poverty Line", 
                  "Percent Population with a Bachelors Degree")

names(scat_x_choices) <- scat_x_names

region_choices <- (cities %>%
                     count(region))$region

city_choices <- as.list(c("None", cities$name_simp))
names(city_choices) <- c("None", cities$name_simp)


#map choices

variable_choices <- as.list(names(cities_long)[9:22])
map_var_choices <- c("Median Income ($)",
                     "Median Home Value ($)",
                     "Median Age Male (Years)", 
                     "Median Age Female (Years)",
                     "Population",
                     "Percent White Population",
                     "Percent Black Population",
                     "Percent Foreign Born Population",
                     "Percent Married Couple Households (of total households)",
                     "Houses for Sale (Per Capita)",
                     "Percent Only English-Speaking Population",
                     "Vehicles per Household",
                     "Percent of Population Below Poverty Line",
                     "Percent Population with a Bachelors Degree")

names(variable_choices) <- map_var_choices

map_year_choices <- as.numeric(c("2013", "2014", "2015", "2016", "2017", "2018"))


ui <- fluidPage(
  setBackgroundImage(
    src = "https://s7.bluegreenvacations.com/is/image/BGV/collection-cityscape-sm?$bg2-hero-sm$"),
  
  h1("US Metro Areas, part II"),
  h5("Grace, Mike, Rodrigo, and Steedman"),
  
  navlistPanel(widths = c(3,9),
               
               #map
               tabPanel(title = "Map",
                        
                        h3("How are variables of interest distributed spatially across top US metro areas?", 
                           br(),
                           "And how have they changed over time?"),
                        
                        selectInput(inputId = "var"
                                    , label = "Choose a variable of interest:"
                                    , choices = variable_choices),
                        selectInput(inputId = "year"
                                    , label = "Choose a year of interest:"
                                    , choices = map_year_choices),
                        leafletOutput("map", height = 630)),
               
               #model
               tabPanel(title = "Model",
                        
                        h2("Which variables are significant predictors of population growth?"),
                        
                        selectInput(inputId = "x"
                                    , label = "Choose a predictor variable of interest:"
                                    , choices = scat_x_choices),
                        selectInput(inputId = "city"
                                    , label = "Identitfy a city in the scatterplot:"
                                    , choices = city_choices),
                        checkboxGroupInput(inputId = "region"
                                           , label = "Choose a region of the U.S:"
                                           , choices = region_choices
                                           , selected = region_choices
                                           , inline = TRUE),
                        
                        textOutput(outputId = "significance"),
                        plotOutput(outputId = "scatter"),
                        verbatimTextOutput(outputId = "model")
               )
               
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
                       , popup= paste0(use_data_map()$name_simp,", ", 
                                       use_data_map()$state_abbrev, "<br>",
                                       map_var_choices[variable_choices == input$var], 
                                       ": ", round(use_data_map()$interest_var, 3))
                       , stroke = FALSE 
                       , radius = 7
                       , fillColor = ~pal()(use_data_map()$interest_var)
                       , fillOpacity = 1.0) %>%
      
      addLegend(pal = pal(), 
                values = use_data_map()$interest_var, 
                position = "topright", 
                title = paste0("Year: ", input$year, "<br>", "<br>", 
                               map_var_choices[variable_choices == input$var]),
                opacity = 1.0)
  })
  
  ##model output
  
  use_data_model <- reactive({
    data <- cities %>%
      filter(region%in%input$region)
  })
  
  city_filter <- reactive({
    data <- filter(cities, name_simp == input$city)
    
  })
  
  #scatterplot with line of best fit
  output$scatter <- renderPlot({
    ggplot(data = use_data_model(), aes_string(x = input$x, y = "pct_pop_change")) +
      geom_point(aes(color = region, size = population)) +
      geom_smooth(method = 'lm', se = FALSE) +
      labs(x = names(scat_x_choices)[scat_x_choices == input$x]
           ,y = "Percent Population Change 2013-2018") +
      geom_label(data = city_filter(), aes(label = name_simp)) + 
      theme_bw()
  })
  
  #print verbatim text for model output
  modsum <- reactive({
    mod <- summary(lm(as.formula(paste("pct_pop_change ~ ", input$x)), data = cities))
  })
  
  output$model <- renderPrint({
    print(modsum())
  })
  
  #print whether x is a significant predictor
  output$significance <- renderText({
    pval <- modsum()$coeff[input$x,"Pr(>|t|)"]
    if(pval < 0.05) {paste0("According to our model, ", names(scat_x_choices)[scat_x_choices == input$x], 
                            " IS a significant predictor of Percent Population Change")}
    else{paste0("According to our model, ", names(scat_x_choices)[scat_x_choices == input$x], " is NOT 
                  a significant predictor of Percent Population Change")}
  })
}

# call to shinyApp
shinyApp(ui = ui, server = server)

