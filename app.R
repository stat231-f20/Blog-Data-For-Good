library(tidyverse) 
library(datasets)
library(viridis)
library(maps)
library(leaflet)

path_in <- "/Users/glecates/Git/Blog-Data-For-Good"
city_data <- read_csv(paste0(path_in, "/dataset.csv"))

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
                   , popup= paste0(city_data$name_simp,": ",city_data$y2013_population)
                   , stroke = FALSE
                   , radius = 5
                   , fillOpacity = 0.9)

