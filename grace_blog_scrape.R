library(tidyverse) 
library(rvest)
library(robotstxt)
library(tidycensus)

path <- "/Users/glecates/TidyCensus"
key <- readLines(paste0(path, "/api_key_UScensus.txt"))
census_api_key(key, install = TRUE, overwrite = TRUE)

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