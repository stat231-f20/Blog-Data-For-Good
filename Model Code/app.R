library(tidyverse)
library(shiny)

#path_in <- "/Users/steedmanjenkins/git/Blog-Data-For-Good/"
path_in <- "/Users/steedmanjenkins/git/Blog-Data-For-Good/"

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
                           y2016_num_vehicles_avail + y2017_num_vehicles_avail + y2018_num_vehicles_avail)/6
         )%>%
  #don't need individual year data anymore
  select(NAME, name_simp, state_abbrev, state, region, latitude, longitude, pct_pop_change, pop_change, population, median_age_male, 
         median_age_female,white_alone, black_alone, foreign_born, total_hh, married_hh, 
         median_income, houses_for_sale, median_value, only_english, num_vehicles)%>%
  #get standardized/per capita variables
  mutate(pct_white = white_alone/population,
         pct_black = black_alone/population,
         pct_foreign_born = foreign_born/population,
         pct_married_hh = married_hh/total_hh,
         per_capita_for_sale = houses_for_sale/population,
         pct_only_english = only_english/population,
         vehicles_per_capita = num_vehicles/total_hh)
cities$region = ifelse(cities$state_abbrev == "PR", "South", cities$region)

scat_x_choices <- as.list(names(cities)[c(11,12,18,20, 23:29)])
scat_x_names <- c("Median Age (Male)", "Median Age (Female)", "Median Income", 
                  "Median Home Value", "Percent White", 
                  "Percent Black", "Percent Foreign Born", "Percent Married Couple Households", 
                  "Houses for Sale (Per Capita)", "Percent Only English-speaking", "Vehicles per Household")
names(scat_x_choices) <- scat_x_names

region_choices <- (cities %>%
                          count(region))$region

city_choices <- as.list(c("None", cities$name_simp))
names(city_choices) <- c("None", cities$name_simp)

ui <- fluidPage(
  
  h1("Metro Areas, part II"),
  h5("Grace, Mike, Rodrigo and Steedman"),
  
  navlistPanel(widths = c(3,9),
    
    #grace (scatterplot ui code)
    tabPanel(title = "Which variables are significant predictors of population change?",
             
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
             
             plotOutput(outputId = "scatter"),
             textOutput(outputId = "significance"),
             verbatimTextOutput(outputId = "model")
    )
  )
)

server <- function(input,output){
  
  use_data <- reactive({
    data <- cities %>%
      filter(region%in%input$region)
  })
  
  city_filter <- reactive({
    data <- filter(cities, name_simp == input$city)
      
  })
  
  output$scatter <- renderPlot({
    ggplot(data = use_data(), aes_string(x = input$x, y = "pct_pop_change")) +
      geom_point(aes(color = region, size = population)) +
      geom_smooth(method = 'lm', se = FALSE) +
      labs(x = names(scat_x_choices)[scat_x_choices == input$x]
           ,y = "Percent Population Change 2013-2018") +
      geom_label(data = city_filter(), aes(label = name_simp)) + 
      theme_bw()
  })
  
  modsum <- reactive({
    mod <- summary(lm(as.formula(paste("pct_pop_change ~ ", input$x)), data = cities))
  })
  
  output$model <- renderPrint({
    print(modsum())
  })
  
  output$significance <- renderText({
    pval <- modsum()$coeff[input$x,"Pr(>|t|)"]
    if(pval < 0.05) {paste0(names(scat_x_choices)[scat_x_choices == input$x], 
           " is a significant predictor of Percent Population Change")}
    else{paste0(names(scat_x_choices)[scat_x_choices == input$x], " is not 
                  a significant predictor of Percent Population Change")}
  })
}

shinyApp(ui = ui, server = server)  


         