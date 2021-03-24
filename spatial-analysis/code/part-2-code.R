## Package setup

pkgs <- c("tidyverse", "tidycensus", "spdep", "mapview", 
          "leafsync", "tmap", "tmaptools", "remotes")

install.packages(pkgs)
remotes::install_github("walkerke/crsuggest")

## Set your Census API key if need be
library(tidycensus)
options(tigris_use_cache = TRUE)
census_api_key("YOUR KEY GOES HERE")


## ----basic-usage---------------------------------------------------------------------------------------
library(tigris)

or_counties <- counties(state = "OR")

or_counties


## ----basic-plot----------------------------------------------------------------------------------------
plot(or_counties$geometry)


## ----benton-tracts-------------------------------------------------------------------------------------
benton_tracts <- tracts(state = "OR", county = "Benton")

plot(benton_tracts$geometry)


## ----benton-roads--------------------------------------------------------------------------------------
benton_roads <- roads(state = "OR", county = "Benton")

plot(benton_roads$geometry)


## ----dc-landmarks--------------------------------------------------------------------------------------
dc_landmarks <- landmarks("DC", type = "point")

plot(dc_landmarks$geometry)


## ----michigan-tiger------------------------------------------------------------------------------------
mi_counties <- counties("MI")

plot(mi_counties$geometry)


## ----michigan-cb---------------------------------------------------------------------------------------
mi_counties_cb <- counties("MI", cb = TRUE)

plot(mi_counties_cb$geometry)


## ----get-yearly-data-----------------------------------------------------------------------------------
tarrant90 <- suppressMessages(tracts("TX", "Tarrant", cb = TRUE, year = 1990))
tarrant00 <- suppressMessages(tracts("TX", "Tarrant", cb = TRUE, year = 2000))
tarrant10 <- tracts("TX", "Tarrant", cb = TRUE, year = 2010)
# Cartographic boundary files not yet released for 2020
tarrant20 <- tracts("TX", "Tarrant", year = 2020)



## ----plot-yearly-data----------------------------------------------------------------------------------
par(mfrow = c(2, 2))

plot(tarrant90$geometry, main = "1990")
plot(tarrant00$geometry, main = "2000")
plot(tarrant10$geometry, main = "2010")
plot(tarrant20$geometry, main = "2020")




## ----mapview, eval = FALSE-----------------------------------------------------------------------------
library(mapview)

mapview(tarrant20)


## ----sync, eval = FALSE--------------------------------------------------------------------------------
library(leafsync)

sync(mapview(tarrant90), mapview(tarrant20))



## ----combine-bgs---------------------------------------------------------------------------------------
library(tidyverse)

state_codes <- c(state.abb, "DC")

us_bgs <- map_df(state_codes, ~block_groups(state = .x, cb = TRUE))

glimpse(us_bgs)


## ----check-crs-----------------------------------------------------------------------------------------
library(sf)

fl_counties <- counties("FL", cb = TRUE)

st_crs(fl_counties)


## ----suggest-crs---------------------------------------------------------------------------------------
library(crsuggest)
library(tidyverse)

fl_crs <- suggest_crs(fl_counties)

glimpse(fl_crs)


## ----transform-crs-------------------------------------------------------------------------------------
fl_projected <- st_transform(fl_counties, crs = 3086)

head(fl_projected)


## ----tidycensus-geometry-------------------------------------------------------------------------------
library(tidycensus)
options(tigris_use_cache = TRUE)

dc_income1 <- get_acs(geography = "tract", 
                     variables = c(hhincome = "B19013_001"), 
                     state = "DC")

dc_income <- get_acs(geography = "tract", 
                     variables = c(hhincome = "B19013_001"), 
                     state = "DC", 
                     geometry = TRUE)



## ----show-geometry-------------------------------------------------------------------------------------
dc_income


## ----plot-geometry-------------------------------------------------------------------------------------
plot(dc_income["estimate"])


## ----geom-sf-------------------------------------------------------------------------------------------
library(tidyverse)

dc_map <- ggplot() + 
  geom_sf(data = dc_income, aes(fill = estimate, geometry = geom))


## ----plot-geom-sf--------------------------------------------------------------------------------------
dc_map


## ----get-hennepin-data---------------------------------------------------------------------------------
hennepin_race <- get_acs(
  geography = "tract",
  state = "MN",
  county = "Hennepin",
  variables = c(White = "B03002_003",
                Black = "B03002_004",
                Native = "B03002_005",
                Asian = "B03002_006",
                Hispanic = "B03002_012"),
  summary_var = "B03002_001",
  geometry = TRUE
) %>%
  mutate(percent = 100 * (estimate / summary_est))


## ----glimpse-hennepin-data-----------------------------------------------------------------------------
glimpse(hennepin_race)


## ----polygons-map, echo = FALSE------------------------------------------------------------------------
library(tmap)

hennepin_black <- filter(hennepin_race, 
                         variable == "Black")

tm_shape(hennepin_black) + 
  tm_polygons() 


## ----choropleth-show, echo = FALSE---------------------------------------------------------------------
tm_shape(hennepin_black) + 
  tm_polygons(col = "percent")


## ----custom-choropleth-show, echo = FALSE--------------------------------------------------------------
tm_shape(hennepin_black, 
         projection = sf::st_crs(26915)) + 
  tm_polygons(col = "percent",
          style = "quantile",
          n = 7,
          palette = "Purples",
          title = "ACS estimate") + 
  tm_layout(title = "Percent Black\nby Census tract",
            frame = FALSE,
            legend.outside = TRUE)


## ----jenks-show, echo = FALSE--------------------------------------------------------------------------
tm_shape(hennepin_black, 
         projection = sf::st_crs(26915)) + 
  tm_polygons(col = "percent",
          style = "jenks",
          n = 7,
          palette = "viridis",
          title = "ACS estimate",
          legend.hist = TRUE) + 
  tm_layout(title = "Percent Black population\nby Census tract",
            frame = FALSE,
            legend.outside = TRUE)


## ----bubbles-code--------------------------------------------------------------------------------------
symbol_map <- tm_shape(hennepin_black) + 
  tm_polygons() + 
  tm_bubbles(size = "estimate", alpha = 0.5, 
             col = "navy")


## ----bubbles-map---------------------------------------------------------------------------------------
symbol_map


## ----facet-map-code------------------------------------------------------------------------------------
facet_map <- tm_shape(hennepin_race,
         projection = sf::st_crs(26915)) + 
  tm_facets(by = "variable", scale.factor = 4) + 
  tm_fill(col = "percent",
          style = "quantile",
          n = 7,
          palette = "Blues")


## ----facet-map-----------------------------------------------------------------------------------------
facet_map


## ----generate-dots, echo = TRUE, eval = FALSE----------------------------------------------------------
groups <- unique(hennepin_race$variable)

hennepin_dots <- map_df(groups, ~{
  hennepin_race %>%
    filter(variable == .x) %>%
    st_transform(26915) %>%
    mutate(est50 = as.integer(estimate / 50)) %>%
    st_sample(size = .$est50, exact = TRUE) %>%
    st_sf() %>%
    mutate(group = .x)
}) %>%
  slice_sample(prop = 1)


## ----plot-dots, eval = FALSE---------------------------------------------------------------------------
tm_shape(hennepin_dots) +
  tm_dots(col = "group", palette = "Set1", size = 0.005)


## ----shift-geo-----------------------------------------------------------------------------------------
us_median_age <- get_acs(geography = "state",
                         variables = "B01002_001",
                         year = 2019,
                         survey = "acs1",
                         geometry = TRUE,
                         shift_geo = TRUE)


## ----show-shift-geo, fig.width = 8---------------------------------------------------------------------
tm_shape(us_median_age) + tm_polygons()


## ----style-shift-geo, fig.width = 10-------------------------------------------------------------------
tm_shape(us_median_age) + 
  tm_polygons(col = "estimate", palette = "RdPu", title = "Median age") + 
  tm_layout(legend.outside = TRUE)


## ---- eval = FALSE-------------------------------------------------------------------------------------
library(mapview)

mapview(dc_income, zcol = "estimate")

m1 <- mapview(dc_income, zcol = "estimate")

htmlwidgets::saveWidget(m1, "dc_income_map.html")


## ----write-shp, eval = FALSE---------------------------------------------------------------------------
library(sf)

st_write(dc_income, "data/dc_income.shp")


## ----get-vax-data--------------------------------------------------------------------------------------
library(tidyverse)

tx_vaccinations <- read_rds("spatial-analysis/data/tx_vaccinations.rds")

glimpse(tx_vaccinations)


## ----get-pop-normalization-----------------------------------------------------------------------------
library(tidycensus)
options(tigris_use_cache = TRUE)

pop16up <- get_acs(
  geography = "zcta",
  variables = "DP03_0001",
  geometry = TRUE,
  state = "TX"
)


## ----map-normalization-pop-----------------------------------------------------------------------------
library(tmap)

tm_shape(pop16up) + tm_fill(col = "estimate")


## ----calculate-rates-----------------------------------------------------------------------------------
tx_vacc_rate <- pop16up %>%
  left_join(tx_vaccinations, by = c("GEOID" = "zip_code")) %>%
  mutate(pct_vaccinated = 100 * (total_vaccinations / estimate)) %>%
  mutate(pct_vaccinated = ifelse(pct_vaccinated > 100, NA, pct_vaccinated))


## ----map-rates-----------------------------------------------------------------------------------------
tm_shape(tx_vacc_rate) + tm_fill(col = "pct_vaccinated", palette = "Reds")


## ----dallas-area---------------------------------------------------------------------------------------
dallas_area <- tx_vacc_rate %>%
  filter(str_sub(GEOID, 1, 2) == "75")

above_40 <- dallas_area %>%
  filter(pct_vaccinated >= 40)


## ----above-40------------------------------------------------------------------------------------------
tm_shape(dallas_area) + tm_polygons() + 
  tm_shape(above_40) + tm_polygons(col = "navy") 


## ----dfw-metro-----------------------------------------------------------------------------------------
dfw_metro <- core_based_statistical_areas(cb = TRUE) %>%
  filter(str_detect(NAME, "Dallas"))


## ----spatial-subset------------------------------------------------------------------------------------
library(sf)

dfw_zips <- tx_vacc_rate[dfw_metro, ]


## ----spatial-filter------------------------------------------------------------------------------------
dfw_zips <- st_filter(tx_vacc_rate, dfw_metro)


## ----plot-filter---------------------------------------------------------------------------------------
tm_shape(dfw_zips) + 
  tm_polygons(col = "pct_vaccinated", palette = "Reds") + 
  tm_layout(legend.outside = TRUE)


## ----get-within----------------------------------------------------------------------------------------
dfw_zips_within <- st_filter(tx_vacc_rate, dfw_metro, .predicate = st_within)


## ----plot-within---------------------------------------------------------------------------------------
tm_shape(dfw_zips_within) + 
  tm_polygons(col = "pct_vaccinated", palette = "Reds") + 
  tm_layout(legend.outside = TRUE)


## ----join-metros---------------------------------------------------------------------------------------
tx_metros <- core_based_statistical_areas(cb = TRUE) %>%
  filter(GEOID %in% c("19100", "26420", "41700", "12420")) %>%
  select(metro_name = NAME)

zips_by_metro <- tx_vacc_rate %>%
  st_join(tx_metros, left = FALSE)


## ----glimpse-zips-by-metro-----------------------------------------------------------------------------
glimpse(zips_by_metro)


## ----compare-metros-code, eval = FALSE-----------------------------------------------------------------
ggplot(zips_by_metro) +
  geom_density(aes(x = pct_vaccinated), color = "navy", fill = "navy",
               alpha = 0.4) +
  theme_minimal() +
  facet_wrap(~metro_name) +
  labs(title = "Percent of population age 16+ vaccinated for COVID-19",
       subtitle = "ZCTAs, largest metropolitan areas in Texas",
       y = "Kernel density estimate",
       x = "Percent receiving at least one vaccine dose")

## ----skater-data-setup---------------------------------------------------------------------------------
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


## ----setup-skater--------------------------------------------------------------------------------------
library(spdep)
set.seed(123456)

input_vars <- input_tracts %>%
  select(median_ageE:pct_collegeE) %>%
  st_drop_geometry() %>%
  scale() %>%
  as.data.frame()

nb <- poly2nb(input_tracts)
costs <- nbcosts(nb, input_vars)
weights <- nb2listw(nb, costs, style = "B")


## ----run-skater----------------------------------------------------------------------------------------
mst <- mstree(weights)

regions <- skater(mst[,1:2], input_vars, ncuts = 6)

input_tracts$group <- as.character(regions$group)


## ----plot-groups---------------------------------------------------------------------------------------
tm_shape(input_tracts) + tm_polygons("group", palette = "Set1")


## ----summarize-regions---------------------------------------------------------------------------------
wayne_regions <- input_tracts %>%
  group_by(group) %>%
  summarize(across(.cols = where(is.numeric),
                   .fns = mean))


## ----wayne-regions-map, fig.width = 10-----------------------------------------------------------------
tm_shape(wayne_regions) + tm_polygons(col = "group", palette = "Set1")


## ----wayne-regions-------------------------------------------------------------------------------------
wayne_regions


## ------------------------------------------------------------------------------------------------------
texas_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "TX",
  geometry = TRUE
)

