## ----install-packages---------------------------------------------
install.packages(c("tidycensus", "tidyverse", "plotly"))


## ----api-key------------------------------------------------------
library(tidycensus)

census_api_key("YOUR KEY GOES HERE", install = TRUE)


## ----decennial------------------------------------------------------------------
pop10 <- get_decennial(
  geography = "state",
  variables = "P001001"
)


## ----view-decennial-------------------------------------------------------------
pop10


## ----acs------------------------------------------------------------------------
income_15to19 <- get_acs(
  geography = "state",
  variables = "B19013_001"
)


## ----view-acs-------------------------------------------------------------------
income_15to19


## ----acs1-----------------------------------------------------------------------
income_19 <- get_acs(
  geography = "state",
  variables = "B19013_001",
  survey = "acs1"
)

income_19


## ----acs-table------------------------------------------------------------------
age_table <- get_acs(
  geography = "state", 
  table = "B01001"
)


## ----view-table-----------------------------------------------------------------
age_table


## ----query-by-state-------------------------------------------------------------
wi_income <- get_acs(
  geography = "county", 
  variables = "B19013_001", 
  state = "WI",
  year = 2016
)

wi_income


## ----query-by-county------------------------------------------------------------
dane_income <- get_acs(
  geography = "tract", 
  variables = "B19013_001", 
  state = "WI", 
  county = "Dane"
)

dane_income


## ----search-variables---------------------------------------------
vars <- load_variables(2019, "acs5")

View(vars)



## ----tidy-data------------------------------------------------------------------
hhinc <- get_acs(
  geography = "state", 
  table = "B19001", 
  survey = "acs1"
)

hhinc


## ----wide-data------------------------------------------------------------------
hhinc_wide <- get_acs(
  geography = "state", 
  table = "B19001", 
  survey = "acs1", 
  output = "wide"
)

hhinc_wide


## ----named-variables------------------------------------------------------------
ga_wide <- get_acs(
  geography = "county",
  state = "GA",
  variables = c(median_income = "B19013_001",
                median_age = "B01002_001"),
  output = "wide"
)

ga_wide


## ----tidyverse------------------------------------------------------------------
library(tidyverse)

tidyverse_logo()


## ----median-age-----------------------------------------------------------------
library(tidycensus)
library(tidyverse)

median_age <- get_acs(
  geography = "county",
  variables = "B01002_001"
)



## ----sort-ascending-------------------------------------------------------------
arrange(median_age, estimate)



## ----sort-descending------------------------------------------------------------
arrange(median_age, desc(estimate))


## ----filter-above-50------------------------------------------------------------
above50 <- filter(median_age, estimate >= 50)

above50


## ----summary-variable-----------------------------------------------------------
race_vars <- c(
  White = "B03002_003",
  Black = "B03002_004",
  Native = "B03002_005",
  Asian = "B03002_006",
  HIPI = "B03002_007",
  Hispanic = "B03002_012"
)

az_race <- get_acs(
  geography = "county",
  state = "AZ",
  variables = race_vars,
  summary_var = "B03002_001"
)


## ----view-summary-variable------------------------------------------------------
az_race


## ----mutate-and-select----------------------------------------------------------
az_race_percent <- az_race %>%
  mutate(percent = 100 * (estimate / summary_est)) %>%
  select(NAME, variable, percent)


## ----view-percent---------------------------------------------------------------
az_race_percent


## ----largest-group--------------------------------------------------------------
largest_group <- az_race_percent %>%
  group_by(NAME) %>%
  filter(percent == max(percent))


## ----view-largest-group---------------------------------------------------------
largest_group


## ----median-by-group------------------------------------------------------------
az_race_percent %>%
  group_by(variable) %>%
  summarize(median_pct = median(percent))


## ----moe-example----------------------------------------------------------------
vars <- paste0("B01001_0", c(20:25, 44:49))

salt_lake <- get_acs(
  geography = "tract",
  variables = vars,
  state = "Utah",
  county = "Salt Lake",
  year = 2019
)

example_tract <- salt_lake %>%
  filter(GEOID == "49035100100")


## ----view-moe-------------------------------------------------------------------
example_tract %>% select(-NAME)


## ----moe-prop-------------------------------------------------------------------
moe_prop(25, 100, 5, 3)


## ----summarize-moe--------------------------------------------------------------
salt_lake_grouped <- salt_lake %>%
  mutate(sex = if_else(str_sub(variable, start = -2) < "26",
                       "Male", 
                       "Female")) %>%
  group_by(GEOID, sex) %>%
  summarize(sum_est = sum(estimate), 
            sum_moe = moe_sum(moe, estimate))



## ----view-summarize-moe---------------------------------------------------------
salt_lake_grouped


## ----get-metro-data-------------------------------------------------------------
library(tidycensus)
library(tidyverse)

metros <-
  get_acs(
    geography = "cbsa",
    variables = "DP03_0021P",
    summary_var = "B01003_001",
    survey = "acs1"
  ) %>%
  filter(min_rank(desc(summary_est)) < 21)


## ----view-metro-data------------------------------------------------------------
glimpse(metros)


## ----first-plot---------------------------------------------------
p <- ggplot(metros, aes(x = NAME, y = estimate)) +
 geom_col()

p


## ----better-plot--------------------------------------------------
p <- metros %>%
 mutate(NAME = str_remove(NAME, "-.*$")) %>%
 mutate(NAME = str_remove(NAME, ",.*$")) %>%
 ggplot(aes(y = reorder(NAME, estimate), x = estimate)) +
 geom_col()


p


## ----best-plot----------------------------------------------------
p <- p +
 theme_minimal() +
 labs(title = "Percentage of commuters who take public transportation to work",
      subtitle = "2019 1-year ACS estimates for the 20 largest US metropolitan areas",
      y = "",
      x = "ACS estimate (percent)",
      caption = "Source: ACS Data Profile variable DP03_0021P via the tidycensus R package")


p



## ----maine-data-----------------------------------------------------------------
maine_income <- get_acs(
  state = "Maine",
  geography = "county",
  variables = c(hhincome = "B19013_001")
) %>%
  mutate(NAME = str_remove(NAME, " County, Maine"))



## ----view-maine-data------------------------------------------------------------
maine_income %>% arrange(desc(moe))



## ----moe-plot-----------------------------------------------------
ggplot(maine_income, aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(size = 3, color = "darkgreen") +
  labs(title = "Median household income",
       subtitle = "Counties in Maine",
       x = "2015-2019 ACS estimate",
       y = "") +
  scale_x_continuous(labels = scales::dollar)


## ----utah-data------------------------------------------------------------------
utah <- get_estimates(
  geography = "state",
  state = "UT",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  year = 2019
) 

utah


## ----prep-utah-data-------------------------------------------------------------
utah_filtered <- filter(utah, str_detect(AGEGROUP, "^Age"), 
                  SEX != "Both sexes") %>%
  mutate(value = ifelse(SEX == "Male", -value, value))

utah_filtered


## ----first-pyramid------------------------------------------------
ggplot(utah_filtered, aes(x = value, y = AGEGROUP, fill = SEX)) +
  geom_col()


## ----formatted-pyramid--------------------------------------------
utah_pyramid <- ggplot(utah_filtered, aes(x = value, y = AGEGROUP, fill = SEX)) +
  geom_col(width = 0.95, alpha = 0.75) +
  theme_minimal(base_family = "Verdana") +
  scale_x_continuous(labels = function(y) paste0(abs(y / 1000), "k")) +
  scale_y_discrete(labels = function(x) gsub("Age | years", "", x)) +
  scale_fill_manual(values = c("darkred", "navy")) +
  labs(x = "",
       y = "2019 Census Bureau population estimate",
       title = "Population structure in Utah",
       fill = "",
       caption = "Data source: US Census Bureau population estimates & tidycensus R package")

utah_pyramid


## ----plotly-------------------------------------------------------
library(plotly)

ggplotly(utah_pyramid)


## ----beeswarm---------------------------------------
install.packages("ggbeeswarm")

library(ggbeeswarm)

ny_race_income <- get_acs(
  geography = "tract",
  state = "NY",
  county = c("New York", "Bronx", "Queens", "Richmond", "Kings"),
  variables = c(White = "B03002_003",
                Black = "B03002_004",
                Asian = "B03002_006",
                Hispanic = "B03002_012"),
  summary_var = "B19013_001"
) %>%
  group_by(GEOID) %>%
  filter(estimate == max(estimate, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(estimate != 0)

ggplot(ny_race_income, aes(x = variable, y = summary_est, color = summary_est)) +
  geom_quasirandom(alpha = 0.5) +
  coord_flip() +
  theme_minimal() +
  scale_color_viridis_c(guide = FALSE) +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Largest group in Census tract",
       y = "Median household income",
       title = "Household income distribution by largest racial/ethnic group",
       subtitle = "Census tracts, New York City",
       caption = "Data source: 2015-2019 ACS")


## ----geofacet-pyramids------------------------------
install.packages("geofacet")

library(geofacet)

us_pyramid_data <- get_estimates(
  geography = "state",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  year = 2019
) %>%
  filter(str_detect(AGEGROUP, "^Age"),
         SEX != "Both sexes") %>%
  group_by(NAME) %>%
  mutate(prop = value / sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(prop = ifelse(SEX == "Male", -prop, prop))

ggplot(us_pyramid_data, aes(x = prop, y = AGEGROUP, fill = SEX)) +
  geom_col(width = 1) +
  theme_minimal() +
  scale_fill_manual(values = c("darkred", "navy")) +
  facet_geo(~NAME, grid = "us_state_with_DC_PR_grid2",
            label = "code") +
  theme(axis.text = element_blank(),
        strip.text.x = element_text(size = 8)) +
  labs(x = "",
       y = "",
       title = "Population structure by age and sex",
       fill = "",
       caption = "Data source: US Census Bureau population estimates & tidycensus R package")

