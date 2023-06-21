install.packages("ggthemes")

new_carers3 <- 
  readRDS(file = "data/new_carers3.rds")

library(dplyr)
carer_info <- readRDS(file = 'data/carer_info.rds') %>% 
  select(
    carer, 
    sex, 
    age_band, 
    date, 
    n, 
    signal, 
    n_min, 
    n_max, 
    combined_score, 
    deprivation_decile
  )

glimpse(carer_info)
carer_info %>% 
  count(nhs_number, sort = T) %>% 
  count(n)

n_distinct(carer_info$nhs_number)
glimpse(carer_registrations)
dim(carer_registrations)

overall_yearly <- 
  readRDS(file = 'data/overallCarerRegistrations.rds')

overall_yearly %>% 
  as_tibble() %>% 
  pull(gp_population) %>% 
  sum(.)

sex_yearly <- 
  readRDS(file = 'data/sexCarerRegistrations.rds')

sex_yearly %>% 
  as_tibble() %>% 
  pull(gp_population) %>% 
  sum(.)

carer_registrations %>% 
  as_tibble() %>%
  filter(constant == 1) %>% 
  pull(gp_population) %>% 
  sum(.)


carer_registrations <- bind_rows(
  readRDS(file = 'data/overallCarerRegistrations.rds') %>%
    mutate(group = 'total'),
  readRDS(file = 'data/sexCarerRegistrations.rds') %>%
    mutate(group = 'sex'),
  readRDS(file = 'data/ageBandCarerRegistrations.rds') %>%
    mutate(group = 'age_band'),
  readRDS(file = 'data/imdCarerRegistrations.rds') %>%
    mutate(group = 'imd'),
  readRDS(file = 'data/ageBandImdCarerRegistrations.rds') %>%
    mutate(group = 'age_band_imd')
) %>%
  ungroup() %>%
  mutate(
    grouping = coalesce(
      as.character(constant), 
      sex, 
      as.character(age_band), 
      as.character(imd_decile)
    )
  ) %>%
  filter(!is.na(grouping)) %>%
  mutate(grouping = if_else((!is.na(constant)) & (grouping == '1'), 'total', grouping))

0.01*622000*6
