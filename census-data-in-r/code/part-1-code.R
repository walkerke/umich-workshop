## ----setup, include=FALSE--------------------------------------------------------
options(htmltools.dir.version = FALSE)
options(tigris_use_cache = TRUE)
library(tidycensus)

knitr::opts_chunk$set(message = FALSE, warning = FALSE, fig.width = 8)


## ----xaringan-themer, include=FALSE, warning=FALSE-------------------------------
library(xaringanthemer)

style_xaringan(
  title_slide_background_color = "#035004",
  text_color = "black",
  header_color = "#035004",
  inverse_background_color = "#035004",
  text_font_family = "Gotham Narrow",
  header_font_family = "Helvetica",
  header_font_weight = "bold",
  link_color = "#1a730f",
  code_inline_color = "#035004"
)


## ---- eval = FALSE---------------------------------------------------------------
## install.packages(c("tidycensus", "tidyverse"))


## ---- eval = FALSE---------------------------------------------------------------
## library(tidycensus)
## 
## census_api_key("YOUR KEY GOES HERE", install = TRUE)


## --------------------------------------------------------------------------------
pop10 <- get_decennial(
  geography = "state",
  variables = "P001001"
)


## --------------------------------------------------------------------------------
pop10


## --------------------------------------------------------------------------------
income_15to19 <- get_acs(
  geography = "state",
  variables = "B19013_001"
)


## --------------------------------------------------------------------------------
income_15to19


## --------------------------------------------------------------------------------
income_19 <- get_acs(
  geography = "state",
  variables = "B19013_001",
  survey = "acs1"
)

income_19


## --------------------------------------------------------------------------------
age_table <- get_acs(
  geography = "state", 
  table = "B01001"
)

age_table


## --------------------------------------------------------------------------------
wi_income <- get_acs(
  geography = "county", 
  variables = "B19013_001", 
  state = "WI",
  year = 2019
)

wi_income


## --------------------------------------------------------------------------------
dane_income <- get_acs(
  geography = "tract", 
  variables = "B19013_001", 
  state = "WI", 
  county = "Dane"
)

dane_income


## --------------------------------------------------------------------------------
hhinc <- get_acs(
  geography = "state", 
  table = "B19001", 
  survey = "acs1"
)

hhinc


## --------------------------------------------------------------------------------
hhinc_wide <- get_acs(
  geography = "state", 
  table = "B19001", 
  survey = "acs1", 
  output = "wide"
)

hhinc_wide


## --------------------------------------------------------------------------------
ga_wide <- get_acs(
  geography = "county",
  state = "GA",
  variables = c(medinc = "B19013_001",
                medage = "B01002_001"),
  output = "wide"
)

ga_wide


## --------------------------------------------------------------------------------
library(tidycensus)
library(tidyverse)

median_age <- get_acs(
  geography = "county",
  variables = "B01002_001"
)



## --------------------------------------------------------------------------------
arrange(median_age, estimate)



## --------------------------------------------------------------------------------
arrange(median_age, desc(estimate))


## --------------------------------------------------------------------------------
above50 <- filter(median_age, estimate >= 50)

above50


## --------------------------------------------------------------------------------
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


## --------------------------------------------------------------------------------
az_race


## --------------------------------------------------------------------------------
az_race_percent <- az_race %>%
  mutate(percent = 100 * (estimate / summary_est)) %>%
  select(NAME, variable, percent)


## --------------------------------------------------------------------------------
az_race_percent


## --------------------------------------------------------------------------------
largest_group <- az_race_percent %>%
  group_by(NAME) %>%
  filter(percent == max(percent))


## --------------------------------------------------------------------------------
largest_group


## --------------------------------------------------------------------------------
az_race_percent %>%
  group_by(variable) %>%
  summarize(median_pct = median(percent))


## --------------------------------------------------------------------------------
library(tidycensus)
library(tidyverse)

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


## --------------------------------------------------------------------------------
example_tract %>% select(-NAME)


## --------------------------------------------------------------------------------
salt_lake_grouped <- salt_lake %>%
  mutate(sex = if_else(str_sub(variable, start = -2) < "26",
                       "Male", 
                       "Female")) %>%
  group_by(GEOID, sex) %>%
  summarize(sum_est = sum(estimate), 
            sum_moe = moe_sum(moe, estimate))



## --------------------------------------------------------------------------------
salt_lake_grouped


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


## --------------------------------------------------------------------------------
p <- metros %>%
  mutate(NAME = str_replace(NAME, "-.*$", "")) %>%
  mutate(NAME = str_replace(NAME, ",.*$", "")) %>%
  ggplot(aes(y = reorder(NAME, estimate), x = estimate)) + 
  geom_col()


p


## --------------------------------------------------------------------------------
p <- p +  
  theme_minimal() + 
  labs(title = "Percentage of residents who take public transportation to work", 
       subtitle = "2019 1-year ACS estimates for the 20 largest US metropolitan areas", 
       y = "", 
       x = "ACS estimate (percent)", 
       caption = "Source: ACS Data Profile variable DP03_0021P via the tidycensus R package")
  

p



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
ggplot(maine_income, aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) + 
  geom_point(size = 3, color = "darkgreen") + 
  labs(title = "Median household income", 
       subtitle = "Counties in Maine", 
       x = "2015-2019 ACS estimate", 
       y = "") + 
  scale_x_continuous(labels = scales::dollar)


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
ggplot(utah_filtered, aes(y = value, x = AGEGROUP, fill = SEX)) + 
  geom_bar(stat = "identity") + 
  coord_flip()


## --------------------------------------------------------------------------------
utah_pyramid <- ggplot(utah_filtered, aes(x = AGEGROUP, y = value, fill = SEX)) + 
  geom_bar(stat = "identity", width = 0.95, alpha = 0.75) + 
  theme_minimal(base_family = "Roboto") + 
  scale_y_continuous(labels = function(y) paste0(abs(y / 1000), "k")) + 
  scale_x_discrete(labels = function(x) gsub("Age | years", "", x)) + 
  scale_fill_manual(values = c("darkred", "navy")) + 
  coord_flip() + 
  labs(x = "", 
       y = "2019 Census Bureau population estimate", 
       title = "Population structure in Utah", 
       fill = "", 
       caption = "Data source: US Census Bureau population estimates & tidycensus R package")


## --------------------------------------------------------------------------------
utah_pyramid


## --------------------------------------------------------------------------------
library(plotly)

ggplotly(utah_pyramid)

