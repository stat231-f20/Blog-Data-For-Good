library(tidycensus)
library(tidyverse)
library(mdsr)

path <- "/Users/steedmanjenkins/Documents/"
key <- readLines(paste0(path, "api_key_UScensus.txt"))
census_api_key(key)

v18 <- load_variables(2018, "acs5", cache = TRUE)
v17 <- load_variables(2017, "acs5", cache = TRUE)
v16 <- load_variables(2016, "acs5", cache = TRUE)
v15 <- load_variables(2015, "acs5", cache = TRUE)
v14 <- load_variables(2014, "acs5", cache = TRUE)
v13 <- load_variables(2013, "acs5", cache = TRUE)
v12 <- load_variables(2012, "acs5", cache = TRUE)
v11 <- load_variables(2011, "acs5", cache = TRUE)
v10 <- load_variables(2010, "acs5", cache = TRUE)
v09 <- load_variables(2009, "acs5", cache = TRUE)

geo <- "metropolitan statistical area/micropolitan statistical area"
population = "B01003_001"
median_income = "B19326_001"

d18 <- get_acs(geography = geo, variables = c(population, median_income), year = 2018)%>%
  pivot_wider(id_cols = NAME, names_from = variable, values_from = estimate)%>%
  rename(population = "B01003_001",
         median_income = "B19326_001")%>%
  arrange(desc(population))%>%
  head(100)%>%
  separate(col = NAME, into = c("name_simp", "name_extra"), sep = ("-|,|/"), remove = FALSE)%>%
  select(-(name_extra))%>%
  #get state and region info 
  extract(col = NAME, into = "state_abbrev"
          , regex = "([A-Z][A-Z])"
          , remove = FALSE) %>%
  left_join(fivethirtyeight::state_info, by = "state_abbrev")%>%
  left_join(latlong, by = c("name_simp" = "name"))
  #fix namesfor:
  #New York, Honolulu, Boise, Winston-Salem
  #add missing latlong for:
  #San Antonio(29.4241° N, 98.4936° W), San Juan(18.4655° N, 66.1057° W), 
  #Virginia Beach(36.8529° N, 75.9780° W), McAllen (26.2034° N, 98.2300° W)

  
  

latlong <- mdsr::WorldCities%>%
  filter(country == "US")
