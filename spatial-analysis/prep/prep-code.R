library(tigris)
library(sf)
options(tigris_use_cache = TRUE)

co <- counties("CO")

st_write(co, "~/data/colorado_counties.shp")

library(readxl)
library(tidyverse)

tx_vaccinations <- read_excel("spatial-analysis/data/TexasCOVID19VaccinesbyZIP.xlsx",
                              sheet = 2) %>%
  set_names(c("zip_code", "total_vaccinations",
              "first_dose", "fully_vaccinated")) %>%
  mutate(across(total_vaccinations:fully_vaccinated, 
                function(x) as.numeric(ifelse(x == "**", NA, x))))

write_rds(tx_vaccinations, "spatial-analysis/data/tx_vaccinations.rds")

library(tidycensus)

x <- load_variables(2019, "acs5/profile")  

pop16up <- get_acs(
  geography = "zcta",
  variables = "DP03_0001",
  geometry = TRUE,
  state = "TX"
)

library(spdep)

input_tracts <- get_acs(
  geography = "tract",
  variables = c(median_age = "B01002_001",
                median_income = "B19013_001",
                pct_college = "DP02_0068P"),
  state = "MI",
  county = "Wayne",
  geometry = TRUE,
  output = "wide"
) %>%
  select(-ends_with("M")) %>%
  na.omit()

input_vars <- input_tracts %>%
  select(median_ageE:pct_collegeE) %>%
  st_drop_geometry() %>%
  scale() %>%
  as.data.frame()

nb <- poly2nb(input_tracts)
costs <- nbcosts(nb, input_vars)
weights <- nb2listw(nb, costs, style = "B")

mst <- mstree(weights)

regions <- skater(mst[,1:2], input_vars, ncuts = 6)

input_tracts$group <- as.character(regions$group)

tm_shape(input_tracts) + tm_polygons("group", palette = "Set1")