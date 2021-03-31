## --------------------------------------------------------------------------------
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


## --------------------------------------------------------------------------------
glimpse(metros)


## --------------------------------------------------------------------------------
p <- ggplot(metros, aes(x = NAME, y = estimate)) + 
  geom_col()

p


ggsave("census-data-in-r/slides/img/metros_step1.png", p)

## --------------------------------------------------------------------------------
p <- metros %>%
  mutate(NAME = str_replace(NAME, "-.*$", "")) %>%
  mutate(NAME = str_replace(NAME, ",.*$", "")) %>%
  ggplot(aes(y = reorder(NAME, estimate), x = estimate)) + 
  geom_col()


p

ggsave("census-data-in-r/slides/img/metros_step2.png", p)



## --------------------------------------------------------------------------------
p <- p +  
  theme_minimal(base_size = 12) + 
  labs(title = "Percentage of commuters who take public transportation to work", 
       subtitle = "2019 1-year ACS estimates for the 20 largest US metropolitan areas", 
       y = "", 
       x = "ACS estimate (percent)", 
       caption = "Source: ACS Data Profile variable DP03_0021P via the tidycensus R package")
  

p

ggsave("census-data-in-r/slides/img/metros_step3.png", p)




## --------------------------------------------------------------------------------
maine_income <- get_acs(
  state = "Maine",
  geography = "county",
  variables = c(hhincome = "B19013_001")
) %>%
  mutate(NAME = str_replace(NAME, " County, Maine", ""))



## --------------------------------------------------------------------------------
maine_income %>% arrange(desc(moe))



## --------------------------------------------------------------------------------
me <- ggplot(maine_income, aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) + 
  geom_point(size = 3, color = "darkgreen") + 
  labs(title = "Median household income", 
       subtitle = "Counties in Maine", 
       x = "2015-2019 ACS estimate", 
       y = "") + 
  scale_x_continuous(labels = scales::dollar)

ggsave("census-data-in-r/slides/img/maine_income.png", me)

## --------------------------------------------------------------------------------
library(tidycensus)
library(tidyverse)

utah <- get_estimates(
  geography = "state",
  state = "UT",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  year = 2019
) 

utah


## --------------------------------------------------------------------------------
utah_filtered <- filter(utah, str_detect(AGEGROUP, "^Age"), 
                  SEX != "Both sexes") %>%
  mutate(value = ifelse(SEX == "Male", -value, value))

utah_filtered


## --------------------------------------------------------------------------------
pm1 <- ggplot(utah_filtered, aes(y = value, x = AGEGROUP, fill = SEX)) + 
  geom_bar(stat = "identity") + 
  coord_flip()

ggsave("census-data-in-r/slides/img/first_pyramid.png", pm1)

## --------------------------------------------------------------------------------
utah_pyramid <- ggplot(utah_filtered, aes(x = value, y = AGEGROUP, fill = SEX)) + 
  geom_col(width = 0.95, alpha = 0.75) + 
  theme_minimal(base_family = "Verdana") + 
  scale_x_continuous(labels = function(x) paste0(abs(x / 1000), "k")) + 
  scale_y_discrete(labels = function(y) gsub("Age | years", "", y)) + 
  scale_fill_manual(values = c("darkred", "navy")) + 
  labs(x = "", 
       y = "2019 Census Bureau population estimate", 
       title = "Population structure in Utah", 
       fill = "", 
       caption = "Data source: US Census Bureau population estimates & tidycensus R package")

ggsave("census-data-in-r/slides/img/utah_pyramid.png", utah_pyramid)

## --------------------------------------------------------------------------------
utah_pyramid


## --------------------------------------------------------------------------------
library(plotly)

ggp <- ggplotly(utah_pyramid) 

htmlwidgets::saveWidget(ggp, "census-data-in-r/slides/img/utah_pyramid2.html")


## --------------------------------------------------------------------------------
library(tidycensus)
library(tidyverse)
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

ggsave("census-data-in-r/slides/img/nyc_beeswarm.png")

## ---------------------------------------------------------------------------------
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

ggplot(us_pyramid_data, aes(x = AGEGROUP, y = prop, fill = SEX)) + 
  geom_bar(stat = "identity", width = 1) + 
  theme_minimal() + 
  scale_y_continuous(labels = function(y) paste0(abs(y / 1000), "k")) + 
  scale_x_discrete(labels = function(x) gsub("Age | years", "", x)) + 
  scale_fill_manual(values = c("darkred", "navy")) + 
  coord_flip() + 
  facet_geo(~NAME, grid = "us_state_with_DC_PR_grid2",
            label = "code") + 
  theme(axis.text = element_blank(),
        strip.text.x = element_text(size = 8)) + 
  labs(x = "", 
       y = "", 
       title = "Population structure by age and sex", 
       fill = "", 
       caption = "Data source: US Census Bureau population estimates & tidycensus R package")

ggsave("census-data-in-r/slides/img/geopyramids.png")