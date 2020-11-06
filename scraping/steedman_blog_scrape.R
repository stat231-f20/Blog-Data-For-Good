library(tidyverse)
library(tidycensus)
library(sf)
library(ggsflabel)
library(scales)
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

years <- lst(2009: 2018)

vars <- c(population = "B01003_001", median_income = "B19326_001", f_born = "B05002_013",
          median_value = "B25077_001", married_hh = "B11001_003", total_hh = "B11001_001",
          houses_for_sale = "B25004_004", white_alone = "B02001_002", black_alone = "B02001_003")

geo <- "metropolitan statistical area/micropolitan statistical area"

latlong <- mdsr::WorldCities%>%
  filter(country == "US")%>%
  select(name, latitude, longitude)

d18 <- get_acs(geography = geo, variables = vars, year = 2018)%>%
  pivot_wider(id_cols = NAME, names_prefix = "y2018_", names_from = variable, values_from = estimate)%>%
  arrange(desc(y2018_population))%>%
  head(100)%>%
  separate(col = NAME, into = c("name_simp", "name_extra"), sep = ("-|,|/"), remove = FALSE)%>%
  select(-(name_extra))%>%
  #get state and region info 
  extract(col = NAME, into = "state_abbrev"
          , regex = "([A-Z][A-Z])"
          , remove = FALSE)

#fix namesfor:
#New York, Honolulu, Boise, Winston-Salem
latlong$name[1706] = "New York"
d18$name_simp[d18$name_simp == "Urban Honolulu"] <- "Honolulu"
d18$name_simp[d18$name_simp == "Boise City"] <- "Boise"
d18$name_simp[d18$name_simp == "Winston"] <- "Winston-Salem"
  
  #join geographic coordinates and state/region 
  d18<- d18%>%
    left_join(latlong, by = c("name_simp" = "name"))%>%
    distinct(NAME, .keep_all = TRUE)%>%
    left_join(fivethirtyeight::state_info, by = "state_abbrev")
  
  #hard code missing coordinates for:
  #San Antonio(29.4241° N, 98.4936° W), San Juan(18.4655° N, 66.1057° W), 
  #Virginia Beach(36.8529° N, 75.9780° W), McAllen (26.2034° N, 98.2300° W)
  d18$latitude[d18$name_simp=="San Antonio"] = 29.4241
  d18$longitude[d18$name_simp=="San Antonio"] = -98.4936
  d18$latitude[d18$name_simp=="San Juan"] = 18.4655
  d18$longitude[d18$name_simp=="San Juan"] = -66.1057
  d18$latitude[d18$name_simp=="Virginia Beach"] = 36.8529
  d18$longitude[d18$name_simp=="Virginia Beach"] = -75.9780
  d18$latitude[d18$name_simp=="McAllen"] = 26.2034
  d18$longitude[d18$name_simp=="McAllen"] = -98.2300


cities <- d18
for (i in 2013:2017){
  df <- get_acs(geography = geo, variables = vars, year = i)%>%
    pivot_wider(id_cols = NAME, names_prefix = paste0("y", i, "_"), names_from = variable, values_from = estimate)
  cities <- cities %>% left_join(df,
            by = "NAME")
}

path_out <- "/Users/steedmanjenkins/git/Blog-Data-For-Good/"
write_csv(cities, paste0(path_out, "dataset.csv"))
