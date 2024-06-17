need_packages <- c(
  'config',
  'tidyverse',
  'lubridate',
  'ggpattern',
  'readxl',
  'odbc'
)

installed <- need_packages %in% installed.packages()
if(length(need_packages[!installed]) > 0) install.packages(need_packages[!installed], type = 'binary')
lapply(need_packages, library, character.only = TRUE)

options(stringsAsFactors = FALSE)

config <- get()

# Load custom functions
invisible(
  lapply(
    Sys.glob('user_defined_functions/*.R'),
    function(x) source(x)
  )
)

patients <- readRDS('patient_df.RDS') %>%
  filter(age > 17)

language_codes <- read.csv('codes/languageReadCodes.csv')

gp_language <- get_query(
  str_replace_all(
    read_file('sql/getFullPrimaryFromReadV3.sql'),
    c(
      '<READCODECTV3>' = paste0("'", paste(language_codes$Code, collapse = "', '"), "'")
    )
  )
)

gp_language <- gp_language %>%
  left_join(
    select(language_codes, read_code_ctv3 = Code, language = Name), 
    by = 'read_code_ctv3'
  )

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

gp_main_language <- gp_language %>%
  filter(!is.na(nhs_number)) %>%
  group_by(nhs_number) %>%
  summarise(
    language = mode(language),
    min_date = min(as_date(gp_date)),
    max_date = max(as_date(gp_date))
  ) %>%
  ungroup() %>%
  mutate(
    language = str_remove(language, 'Main spoken language '),
    language = str_remove(language, 'Using '),
    language = str_remove(language, 'Preferred method of communication: '),
    language = str_replace_all(language, 'British sign language', 'BSL'),
    language = str_replace_all(language, 'Makaton sign language', 'Makaton')
  ) 

patients <- patients %>% 
  left_join(gp_main_language, by = 'nhs_number')

language_year <- patients %>% 
  mutate(language = coalesce(language, 'Unknown')) %>%
  group_by(cambridge_date, carer) %>% 
  summarise(
    n_patients = n(), 
    n_non_english = sum(!language %in% c('English', 'Unknown')), 
    n_unknown = sum(language == 'Unknown'), 
    perct_english = 100 * (n_patients - n_non_english - n_unknown) / n_patients, 
    perct_non_english = 100 * n_non_english / n_patients, 
    perct_unknown = 100 * n_unknown / n_patients,
    perct_assumed_english = 100 * (n_patients - n_non_english) / n_patients,
    lcl = perct_non_english - 
      qnorm(1 - 0.05/2) * sqrt(perct_non_english * (100 - perct_non_english) / n_non_english),
    ucl = perct_non_english +
      qnorm(1 - 0.05/2) * sqrt(perct_non_english * (100 - perct_non_english) / n_non_english)
  ) %>%
  ungroup()

language_year %>%
  ggplot() + 
    geom_line(aes(cambridge_date, perct_english, linetype = carer), size = 1, colour = 'red') + 
    geom_line(aes(cambridge_date, perct_non_english, linetype = carer), size = 1, colour = 'blue') + 
    geom_line(aes(cambridge_date, perct_unknown, linetype = carer), size = 1, colour = 'green') +
    ylim(0, 100)

language_year %>%
  ggplot() + 
  geom_line(aes(cambridge_date, perct_assumed_english, linetype = carer), size = 1, colour = 'red') + 
  geom_line(aes(cambridge_date, perct_non_english, linetype = carer), size = 1, colour = 'blue') + 
  ylim(0, 100)

stop()

language_year <- gp_main_language %>% 
  mutate(
    min_year = year(min_date),
    max_year = year(max_date),
    year = map2(min_year, max_year, seq, by = 1)
  ) %>%
  unnest(year) %>%
  distinct()

language_order <- language_year %>%
  count(year, language) %>%
  group_by(year) %>% 
  mutate(perct = round(100 * n / sum(n), 1)) %>% 
  ungroup() %>%
  mutate(
    language = str_remove(language, 'Main spoken language '),
    language = str_remove(language, 'Using '),
    language = str_remove(language, 'Preferred method of communication: '),
    language = str_replace_all(language, 'British sign language', 'BSL'),
    language = str_replace_all(language, 'Makaton sign language', 'Makaton')
  ) %>%
  group_by(year) %>%
  arrange(year, -perct) %>%
  mutate(
    cum_perct = cumsum(perct),
    position = row_number()
  ) %>%
  ungroup()

english_order <- language_year %>%
  count(year, language = str_detect(language, 'English')) %>%
  group_by(year) %>% 
  mutate(
    perct = round(100 * n / sum(n), 1),
    lcl = perct - 
      qnorm(1 - 0.05/2) * sqrt(perct * (100 - perct) / sum(n)),
    ucl = perct +
      qnorm(1 - 0.05/2) * sqrt(perct * (100 - perct) / sum(n))
  ) %>% 
  arrange(year, -perct) %>%
  mutate(
    cum_perct = cumsum(perct),
    position = row_number()
  ) %>%
  ungroup()

english_order %>%
  filter(between(year, 2016, 2021), position < 6) %>%
  ggplot() +
    geom_line(aes(year, perct, group = language, colour = language)) +
    ylim(0, 100) +
    ylab('English Speakers At Primary Care [%]')

msoa_language_11 <- read_csv("data/census_2011_language.csv") %>% 
  filter(str_detect(geography, 'Leeds')) %>%
  select(
    msoa = `geography code`,
    residents_over_three = `Main Language: All usual residents aged 3 and over; measures: Value`,
    english = `Main Language: English (English or Welsh if in Wales); measures: Value`
  ) %>% 
  mutate(
    non_english = residents_over_three - english, 
    perct_english = english / residents_over_three * 100
  )

msoa_language_21 <- read_csv("data/census_2021_language.csv") %>% 
  filter(str_detect(msoa_name, 'Leeds')) %>%
  select(
    msoa = `geography code`,
    residents_over_three_21 = `Main language (detailed): Total: All usual residents aged 3 years and over`,
    english_21 = `Main language (detailed): English (English or Welsh in Wales)`
  ) %>% 
  mutate(
    non_english_21 = residents_over_three_21 - english_21, 
    perct_english_21 = english_21 / residents_over_three_21 * 100
  )

gp_non_english <- patients %>% 
  mutate(language = coalesce(language, 'Unknown')) %>%
  filter(cambridge_date == ymd('2021-12-01')) %>%
  inner_join(geographr::lookup_lsoa11_msoa11, by = c('lsoa' = 'lsoa11_code')) %>%
  group_by(msoa = msoa11_code) %>%
  summarise(
    n_patients = n(),
    english_patients = sum(language == 'English'),
    non_english_patients = sum(!language %in% c('English', 'Unknown')),
    non_english_perct = 100 * non_english_patients / n_patients
  ) %>%
  ungroup()

msoa_language_sf <- st_read('leeds/leeds.shp') %>%
  group_by(msoa = msoa_code) %>%
  summarise() %>%
  left_join(msoa_language_11, by = 'msoa') %>%
  left_join(msoa_language_21, by = 'msoa') %>%
  left_join(gp_non_english, by = 'msoa')

non_english_perct <- colorNumeric(
  'RdBu', 
  100 - range(
    c(msoa_language_sf$perct_english, msoa_language_sf$perct_english_21),
    na.rm = TRUE
  )
)

non_english_c <- colorNumeric(
  'RdBu',
  msoa_language_sf$non_english_perct
)

msoa_language_sf %>% 
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(
    color = 'black', 
    weight = 0.5, 
    opacity = 1, 
    fillOpacity = 1, 
    fillColor = ~ non_english_perct(100 - perct_english), 
    popup = ~ 100 - perct_english
  )


msoa_language_sf %>% 
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(
    color = 'black', 
    weight = 0.5, 
    opacity = 1, 
    fillOpacity = 1, 
    fillColor = ~ non_english_c(non_english_perct),
    label = ~ paste0('<b>', round(non_english_perct, 1), '%</b>')
  )

#---- Bivariate map ----
bivar2 <- bi_class(
  mutate(msoa_language_sf, non_english_perct_11 = 100 - perct_english), 
  x = non_english_perct_11, 
  y = non_english_perct, 
  style = "fisher", 
  dim = 2
)

bi_pallete <- biscale:::bi_pal_pull('DkCyan', 2, F, F)

bi_colour <- function(x, p = bi_pallete) {
  as.character(p[x])
}

#Figure 9 - bivariate map showing areas with higher or lower rates
bivar2 %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    color = 'black',
    weight = 0.5,
    opacity = 1,
    fillOpacity = 1,
    fillColor = ~ bi_colour(bi_class)
  )

ggplot() +
  geom_sf(data = bivar2,aes(fill = bi_class),color=NA,size = 0.1,show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan", dim = 2) +
  bi_theme(legend.position="bottomright",legend.key.size = unit(0.2, "cm")) +
  theme(title = element_text(size=10))
