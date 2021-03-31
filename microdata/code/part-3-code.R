install.packages(c("survey", "srvyr", "idbr", "inegiR",
                   "cancensus", "geobr", "mapview", "tidyverse", 
                   "remotes", "tmap"))

remotes::install_github("walkerke/tidycensus")
remotes::install_github("diegovalle/mxmaps")
remotes::install_github("Shelmith-Kariuki/rKenyaCensus")

## ----api-key, eval = FALSE------------------------------------------------------------------
## library(tidycensus)
## 
## census_api_key("YOUR KEY GOES HERE", install = TRUE)


## ----first-call, results = 'hide'-----------------------------------------------------------
library(tidycensus)

wy_pums <- get_pums(
  variables = c("SEX", "AGEP", "SCHL"),
  state = "WY",
  survey = "acs1",
  year = 2019
)


## -------------------------------------------------------------------------------------------
wy_pums


## ----tabulate-------------------------------------------------------------------------------
library(tidyverse)

wy_age_50 <- filter(wy_pums, AGEP == 50)

print(sum(wy_pums$PWGTP))
print(sum(wy_age_50$PWGTP))


## ----view, eval = FALSE---------------------------------------------------------------------
## View(pums_variables)


## ----recode, results = 'hide'---------------------------------------------------------------
wy_pums_recoded <- get_pums(
  variables = c("SEX", "AGEP", "SCHL"),
  state = "WY",
  survey = "acs1",
  year = 2019,
  recode = TRUE
)


## ----show-recode----------------------------------------------------------------------------
wy_pums_recoded


## ----filter, results = 'hide'---------------------------------------------------------------
wy_pums_filtered <- get_pums(
  variables = c("SEX", "AGEP", "SCHL"),
  state = "WY",
  survey = "acs5",
  variables_filter = list(
    SEX = 2,
    SCHL = 16:20
  ),
  year = 2019,
  recode = TRUE
)


## ----show-filter----------------------------------------------------------------------------
wy_pums_filtered


## ----plot-pumas-----------------------------------------------------------------------------
library(tigris)
options(tigris_use_cache = TRUE)

wy_pumas <- pumas(state = "WY", cb = TRUE)

plot(wy_pumas$geometry)


## ----get-pumas, results = 'hide'------------------------------------------------------------
wy_age_by_puma <- get_pums(
  variables = c("PUMA", "AGEP"),
  state = "WY",
  survey = "acs5"
)


## ----show-pumas-----------------------------------------------------------------------------
wy_age_by_puma


## ----subset-puma, results = 'hide'----------------------------------------------------------
wy_puma_subset <- get_pums(
  variables = "AGEP",
  state = "WY",
  survey = "acs5",
  puma = "00500"
)


## ----show-subset-puma-----------------------------------------------------------------------
wy_puma_subset


## ----ms-pums, results = 'hide'--------------------------------------------------------------
ms_pums <- get_pums(
  variables = c("PUMA", "SEX", "AGEP", "SCHL"),
  state = "MS",
  survey = "acs5",
  year = 2019,
  recode = TRUE
)

ms_age_sex <- ms_pums %>%
  count(SEX_label, AGEP, wt = PWGTP) 


## ----ms-count-------------------------------------------------------------------------------
ms_age_sex


## ----ms-summary-----------------------------------------------------------------------------
ms_pums_summary <- ms_pums %>% 
  mutate(ba_above = SCHL %in% c("21", "22", "23", "24")) %>% 
  group_by(PUMA, SEX_label) %>% 
  summarize(
    total_pop = sum(PWGTP),
    mean_age = weighted.mean(AGEP, PWGTP),
    ba_above = sum(PWGTP[ba_above == TRUE & AGEP >= 25]),
    ba_above_pct = 100 * (ba_above / sum(PWGTP[AGEP >= 25]))
  )


## ----show-ms-summary------------------------------------------------------------------------
ms_pums_summary


## ----join-ms-data---------------------------------------------------------------------------
library(tigris)
library(tmap)
options(tigris_use_cache = TRUE)

ms_pumas <- pumas("MS", cb = TRUE)

joined_pumas <- ms_pumas %>%
  left_join(ms_pums_summary, by = c("PUMACE10" = "PUMA")) %>%
  filter(SEX_label == "Female")


## ----ms-pums-map----------------------------------------------------------------------------
ms_map <- tm_shape(joined_pumas) + 
  tm_polygons(col = "ba_above_pct", 
              palette = "Greens",
              title = "% of women with\ncollege degree") + 
  tm_layout(legend.outside = TRUE,
            legend.outside.position = "right")


## ----show-ms-map----------------------------------------------------------------------------
ms_map


## ----rep-weights, eval = FALSE--------------------------------------------------------------
ms_pums_replicate <- get_pums(
  variables = c("PUMA", "SEX", "AGEP", "SCHL"),
  state = "MS",
  survey = "acs5",
  year = 2019,
  recode = TRUE,
  rep_weights = "person"
)


## ----show-rep-weights-----------------------------------------------------------------------
ms_pums_replicate


## ----svy-object-----------------------------------------------------------------------------
library(survey)
library(srvyr)

ms_pums_svy <- ms_pums_replicate %>%
  to_survey(type = "person", 
            design = "rep_weights")

class(ms_pums_svy)


## ----svy-count------------------------------------------------------------------------------
ms_pums_svy %>% 
  survey_count(PUMA, SEX_label)


## ----svy-summary----------------------------------------------------------------------------
ms_svy_summary <- ms_pums_svy %>% 
  mutate(ba_above = SCHL %in% c("21", "22", "23", "24")) %>% 
  filter(AGEP >= 25) %>% 
  group_by(PUMA, SEX_label) %>% 
  summarize(
    age_25_up = survey_total(vartype = "se"),
    ba_above_n = survey_total(ba_above, vartype = "se"),
    ba_above_prop = survey_mean(ba_above, vartype = "se")
    )

glimpse(ms_svy_summary)


## ----svy-plot -----------------------------------------------------------------
ms_svy_summary_moe <- ms_svy_summary %>%
  mutate(ba_above_prop_moe = ba_above_prop_se * 1.645) %>%
  filter(SEX_label == "Female")

ggplot(ms_svy_summary_moe, aes(x = ba_above_prop, y = reorder(PUMA, ba_above_prop))) +
  geom_errorbarh(aes(xmin = ba_above_prop - ba_above_prop_moe,
                     xmax = ba_above_prop + ba_above_prop_moe)) +
  geom_point(size = 3, color = "navy") +
  labs(title = "Percent of women age 25+ with 4-year college degree",
       subtitle = "PUMAs in Missisippi.  Error bars represent margin of error at 90 percent confidence level.",
       x = "2015-2019 ACS estimate (from PUMS data)",
       y = "") +
  scale_x_continuous(labels = scales::percent) +
  theme_grey(base_size = 14)


## ----ri-data, results = 'hide'--------------------------------------------------------------
ri_pums_to_model <- get_pums(
  variables = c("PUMA", "WAGP", "JWMNP", "JWTRNS", "COW", "ESR"),
  state = "RI",
  survey = "acs5",
  year = 2019,
  rep_weights = "person"
  )


## ----ri-filter------------------------------------------------------------------------------
ri_model_sd <- ri_pums_to_model %>% 
  mutate(
    emp_type = case_when(
      COW %in% c("1", "2")      ~ "private",
      COW %in% c("3", "4", "5") ~ "public",
      TRUE                      ~ "self"
      )
    ) %>%
  to_survey() %>%
  filter(
    ESR == 1,   # civilian employed
    JWTRNS != 11, # does not work at home
    WAGP > 0,   # earned wages last year
    JWMNP > 0   # commute more than zero min
  ) 


## ----ri-model-------------------------------------------------------------------------------
model <- survey::svyglm(log(JWMNP) ~ log(WAGP) + emp_type + PUMA, 
                        design = ri_model_sd)


## ----ri-model-summary-----------------------------------------------------------------------
summary(model)


## ----idbr-1, eval = FALSE-------------------------------------------------------------------
library(idbr)
library(tidyverse)

se_asia_lex <- idb5(
  country = c("Laos", "Vietnam", "Cambodia", "Thailand"),
  year = 1995:2021,
  variable = "E0",
  country_name = TRUE
)

ggplot(se_asia_lex, aes(x = time, y = E0, color = NAME)) +
  theme_minimal(base_size = 14) +
  geom_line(size = 2) +
  labs(title = "Change in life expectancy, 1995-2021",
       y = "Life expectancy at birth",
       x = "Year")


## ----idbr-2, eval = FALSE-------------------------------------------------------------------
top_tfr <- idb5(
  country = "all",
  year = 2021,
  variable = "TFR",
  country_name = TRUE
) %>%
  slice_max(TFR, n = 10)


## ----show-tfr, eval = FALSE-----------------------------------------------------------------
top_tfr


## ----cancensus-1----------------------------------------------------------------------------
library(cancensus)
library(tidyverse)
library(sf)
options(cancensus.api_key = Sys.getenv("CANCENSUS_API_KEY"))

# View(list_census_vectors("CA16"))



## ----montreal-english, eval = FALSE---------------------------------------------------------
montreal_english <- get_census(
  dataset = "CA16",
  regions = list(CMA = "24462"),
  vectors = "v_CA16_1364",
  level = "CT",
  geo_format = "sf",
  labels = "short"
)


## ----map-montreal-english, eval = FALSE-----------------------------------------------------
tmap_mode("view")

montreal_pct <- montreal_english %>%
  mutate(pct_english = 100 * (v_CA16_1364 / Population))

tm_shape(montreal_pct) +
  tm_polygons(col = "pct_english", alpha = 0.5, palette = "viridis",
              style = "jenks", n = 7, title = "Percent speaking<br/>English at home")


## ----mxmaps, eval = FALSE-------------------------------------------------------------------
## # remotes::install_github("diegovalle/mxmaps")
library(mxmaps)
token_inegi <- Sys.getenv("INEGI_API_KEY")

state_regions <- df_mxstate_2020$region
choropleth_inegi(token_inegi, state_regions,
                 indicator = "3104003001",
                 legend = "%",
                 title = "Percentage born outside\nstate of residence") +
  theme_void(base_size = 18)


## ----inegir---------------------------------------------------------------------------------
library(inegiR)
token_inegi <- Sys.getenv("INEGI_API_KEY")
state_regions <- mxmaps::df_mxstate_2020$region

pct_migrants <- map_df(state_regions, ~{
  inegi_series(series_id = "3104003001", 
               token = token_inegi, 
               geography = .x,
               database = "BISE") %>%
    mutate(state_code = .x)
})
  


## ----show-inegir----------------------------------------------------------------------------
head(pct_migrants)


## ----geobr, results = "hide", eval = FALSE--------------------------------------------------
library(geobr)

rj_tracts <- read_census_tract(code_tract = 3304557)


## ----show-geobr, eval = FALSE---------------------------------------------------------------
mapview::mapview(rj_tracts)


## ----kenya-1--------------------------------------------------------------------------------
# remotes::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus)
library(tidyverse)

religion <- V4_T2.30 %>%
  filter(County != "KENYA") %>%
  mutate(county_title = str_to_title(County), 
         prop_islam = Islam / Total)


## ----plot-kenya, eval = FALSE---------------------------------------------------------------
ggplot(religion, aes(x = prop_islam, y = reorder(county_title, prop_islam))) +
  geom_col(fill = "navy", alpha = 0.6) +
  theme_minimal(base_size = 10) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Percent Muslim by County in Kenya",
       subtitle = "2019 Kenyan Census",
       x = "",
       y = "",
       caption = "Data source: rKenyaCensus R package")


## ----map-kenya, eval = FALSE----------------------------------------------------------------
kenya_islam_map <- KenyaCounties_SHP %>%
  sf::st_as_sf() %>%
  mutate(County = str_remove(County, " CITY")) %>%
  left_join(religion, by = "County") %>%
  mutate(pct_islam = 100 * prop_islam)

tm_shape(kenya_islam_map) +
  tm_polygons(col = "pct_islam",
              palette = "viridis",
              style = "jenks",
              n = 7,
              title = "% Muslim",
              alpha = 0.6)

