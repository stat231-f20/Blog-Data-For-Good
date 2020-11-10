library(tidycensus)
library(tidyverse)
library(fivethirtyeight)
path_in <- "/Users/rodrigo/Git/STAT-231-Rodrigo/data"
key <- readLines(paste0(path_in,"/RodACkey.txt"))

##this is just one year, 2018

rodmap <- read.csv(paste0(path_in, "/uscities.csv"))%>%
  select(city, state_id, lat, lng)%>%
  mutate(names_clean = paste(city, state_id, sep = ", "))

census_api_key(key, overwrite = TRUE)

vars <- c("B01003_001",
          "B992512_001", 
          "B23018_001",
          "B99162_002")

v18 <- load_variables(2018, "acs5", cache = TRUE)
v17 <- load_variables(2017, "acs5", cache = TRUE)
v16 <- load_variables(2016, "acs5", cache = TRUE)
v15 <- load_variables(2015, "acs5", cache = TRUE)
v14 <- load_variables(2014, "acs5", cache = TRUE)
v13 <- load_variables(2013, "acs5", cache = TRUE)


map <- get_acs(
  geography = "metropolitan statistical area/micropolitan statistical area",
  variables = vars,
  year = 2014)%>%
  pivot_wider(id_cols = NAME, names_from = variable, values_from = estimate)%>%
  rename(population = B01003_001,
         vehiclesAvailable = B992512_001,
         hoursWorkedTwelveMonths = B23018_001, #vars. of interest
         speaksOnlyEnglish =  B99162_002)

topAreas <- map%>%
  arrange(desc(population))%>%extract(col = NAME, into = "state"
                                      , regex = "([A-Z][A-Z])"
                                      , remove = FALSE) %>%
  left_join(fivethirtyeight::state_info, by = c("state" = "state_abbrev"))%>%
  #get name into "First City, State abbrev" format
  separate(col = NAME, into = c("name_simp", "name_extra"), sep = ("-|,|/"), remove = FALSE)%>%
  select(-(name_extra))%>%
  mutate(names_clean = paste(name_simp, state, sep = ", "))%>%
  head(150)

joinedMap <- left_join(topAreas, rodmap)%>%
  select(names_clean, city, state_id, division, region, lat, lng, population, hoursWorkedTwelveMonths, speaksOnlyEnglish, vehiclesAvailable)


out_path <- "/users/rodrigo/Git/STAT-231-Rodrigo/homeworks/PUG City"
write_csv(x = joinedMap, path = paste0(out_path,"/rodrigo_cities_scrape.csv"))


#Next steps from here would be implementing:
