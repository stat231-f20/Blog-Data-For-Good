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

path_in <- "/Users/steedmanjenkins/git/Blog-Data-For-Good"
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


