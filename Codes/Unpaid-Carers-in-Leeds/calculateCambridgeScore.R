need_packages <- c(
  'config',
  'readxl',
  'tidyverse',
  'lubridate',
  'odbc'
)

installed <- need_packages %in% installed.packages()
if(length(need_packages[!installed]) > 0) install.packages(need_packages[!installed], type = 'binary')
lapply(need_packages, library, character.only = TRUE)

options(stringsAsFactors = FALSE)
options(dplyr.summarise.inform = FALSE)

# config <- get(file = "../config.yml")
config <- get(file = "config.yml")

# Load custom functions
invisible(
  lapply(
    Sys.glob('user_defined_functions/*.R'),
    function(x) source(x)
  )
)

read_codes <- function(l, d = original_dir) {
  if (length(list.files(path = d[str_detect(d, l)], full.names = TRUE)) == 0) {
    code_df <- tibble()
  } else {
    code_df <- read_csv(
      list.files(
        path = d[str_detect(d, l)], full.names = TRUE
      )[2],
      col_types = cols()
    )
  }
  
  code_df
}

patient_search <- function(link_id, code_list, code_type = read_or_dmd_code) {
  cat('\nSearching', link_id, 'records...')
  
  search_col <- code_type$read_or_dmd[code_type$link == link_id]
  
  tictoc::tic()
  
  query <- get_query(
    str_replace_all(
      ifelse(
        search_col == 'read',
        read_file('sql/getDistinctPrimaryFromReadV2.sql'),
        read_file('sql/getDistinctPrimaryFromDmd.sql')
      ),
      c(
        '<ReadOrDmdCode>' = code_list
      )
    )
  ) %>%
    mutate(code = as.character(code))
  
  cat(' Done!')
  tictoc::toc()
  
  return(query)
}

if (!file.exists('cambridge_grouped2.RDS')) {
  # Directories to look through
  original_dir <- list.dirs(path = 'codes/cambridge_score')
  
  cambridge_link <- read.csv('codes/cambridge_score/cambridge_code_link.csv') %>% 
    rename(
      link = 1, 
      condition = 2, 
      description = 3, 
      provenance = 4, 
      prevalence = 5,
      lookback = 6
    ) %>% 
    mutate(
      link = str_sub(link, 1, 6)
    )
  
  read_or_dmd_code <- tibble(
    link = str_sub(original_dir[-1], 31, 36),
    read_or_dmd = if_else(str_detect(original_dir[-1], '_MC_'), 'read', 'dmd')
  )
  
  cambridge_link <- cambridge_link %>%
    left_join(read_or_dmd_code, by = 'link')
  
  cambridge_link <- cambridge_link %>%
    mutate(
      period = case_when(
        str_detect(lookback, 'ever') ~ years(100),
        str_detect(lookback, '12 months') ~ months(12),
        str_detect(lookback, '5 years') ~ years(5),
        str_detect(lookback, 'last year') ~ years(1),
        TRUE ~ years(100)
      ),
      n_required = case_when(
        str_detect(lookback, '4 or more') & (read_or_dmd == 'dmd') ~ 4,
        str_detect(lookback, 'last 2') ~ 2,
        TRUE ~ 1
      )
    )
  
  # Lookups for prodcode to BNF code
  dmd_gem_link <- read_tsv(
    'codes/cambridge_score/prodcode_to_dmd.txt', 
    col_types = c('ccccccc')
  )
  
  cambridge_link <- cambridge_link %>%
    mutate(codes = map(link, read_codes)) %>%
    unnest(codes) %>%
    mutate(readcode = str_sub(readcode, 1, 5))
  
  cambridge_link <- cambridge_link %>% 
    left_join(select(dmd_gem_link, gemscriptcode, dmdcode), by = 'gemscriptcode')
  
  code_search <- cambridge_link %>%
    filter((str_length(readcode) == 5) | !is.na(dmdcode)) %>%
    mutate(readcode = coalesce(readcode, dmdcode)) %>%
    distinct(link, readcode) %>%
    filter(!is.na(readcode)) %>%
    group_by(link) %>%
    summarise(
      codes = paste0("'", paste(readcode, collapse = "', '"), "'")
    )
  
  cambridge <- code_search %>%
    mutate(patients = map2(link, codes, patient_search)) %>%
    unnest(patients) %>%
    select(-codes)
  
  saveRDS(cambridge, 'cambridge_grouped2.RDS')
  
  rm(code_search)
  gc()
}

cambridge <- readRDS('cambridge_grouped2.RDS')

#---- Weightings ----
condition <- c(
  "Hypertension" = "HYP",
  "Anxiety/Depression" = c("ANX", "ANX", "DEP", "DEP"),
  "Painful condition" = c("PNC", "PNC"),
  "Hearing loss" = "HEL",
  "Irritable bowel syndrome" = c("IBS", "IBS"),
  "Asthma" = c("AST", "AST"),
  "Diabetes" = "DIB",
  "Prostate disorders" = "PRO",
  "Thyroid disorders" = "THY",
  "Coronary heart disease" = "CHD",
  "Chronic kidney disease" = "CKD",
  "Diverticular disease" = "DIV",
  "Chronic sinusitis" = "SIN",
  "Atrial fibrillation" = "ATR",
  "Constipation" = "CON",
  "Stroke & TIA" = c("STR"),
  "COPD" = "COP",
  "Connective tissue disorder" = "RHE",
  "Cancer" = "CAN",
  "Peptic ulcer disease" = "PEP",
  "Alcohol problems" = "ALC",
  "Substance misuse" = "PSM",
  "Psoriasis or eczema" = c("PSO", "PSO"),
  "Blindness and low vision" = "BLI",
  "Heart failure" = "HEF",
  "Dementia" = "DEM",
  "Psychosis/bipolar disorder" = c("SCZ", "SCZ"),
  "Epilepsy" = c("EPI", "EPI"),
  "Inflammatory bowel disease" = "IBD",
  "Peripheral vascular disease" = "PVD",
  "Anorexia or bulimia" = "ANO",
  "Chronic Liver Disease" = "CLD",
  "Migraine" = "MIG",
  "Learning disability" = "LEA",
  "Bronchiectasis" = "BRO",
  "Multiple sclerosis" = "MSC",
  "Parkinson’s disease" = "PRK"
)

conditions <- tibble(
  link = condition,
  condition = str_remove_all(names(condition), '[0-9]')
)

weights <- read_excel('codes/cambridge_score/cambridge_weights.xlsx') %>%
  left_join(conditions, by = 'condition')

calculate_morbidity_score <- function(
  end_date, 
  patient_cambridge_df,
  weight_df = weights
) {
  cat('\n', as.character(end_date), ':')
  
  score_df <- patient_cambridge_df %>%
    filter(gp_date >= (end_date - period)) %>%
    group_by(nhs_number, link) %>%
    summarise(
      n = n(),
      n_required = first(n_required)
    ) %>%
    ungroup() %>%
    filter(n >= n_required) %>%
    left_join(distinct(weight_df), by = 'link') %>%
    group_by(nhs_number) %>%
    summarise(
      score = sum(consultation),
      mortality_score = sum(mortality),
      nel_score = sum(emergency_admission),
      avg_score = sum(avg_weight)
    ) %>%
    ungroup()
  
  score_df <- score_df %>%
    mutate(cambridge_date = end_date)
  
  cat('Done!')
  
  return(score_df)
}

carer_codes <- read_csv('codes/carerReadCodes.csv')

all_carers <- get_query(
  str_replace_all(
    read_file('sql/getPrimaryFromRead.sql'),
    c(
      '<READCODEV2>' = paste0("'", paste(carer_codes$read_code_v2, collapse = "', '"), "'")
    )
  )
)

all_carers <- all_carers %>%
  mutate(care_ended = str_detect(read_code, '918f'))

all_carers2 <- all_carers %>%
  filter(!is.na(nhs_number)) %>%
  group_by(nhs_number) %>%
  mutate(
    start_date = round_date(
      as_date(min(gp_date[!care_ended])), 
      'month'
    ),
    no_end_date = round_date(
      as_date(max(gp_date[!care_ended])) + years(1),
      'month'
    ),
    end_date = round_date(
      coalesce(as_date(max(gp_date[care_ended])), ymd('2021-12-31')),
      'month'
    )
  ) %>%
  ungroup() %>%
  mutate(end_date = coalesce(end_date, no_end_date, ymd('2021-12-01')))

carer_dates <- all_carers2 %>%
  distinct(nhs_number, start_date, end_date)

patients <- get_query(
  'WITH emis AS (
    SELECT 
        Patient_Pseudonym AS nhs_number,
        GenderOfPatient AS sex,
        YearOfBirth AS yob,
        PracticeCode AS gpp_code,
        RegistrationStartDate AS reg_date,
        RegistrationEndDate AS dereg_date,
        DateOfDeath AS dod,
        Patient_LSOA AS lsoa
    FROM 
        [Warehouse_LDMPC].[dbo].[EMIS_Patients]
  WHERE
    PatientType = 4
  ),
  systmone AS (
    SELECT 
        Patient_Pseudonym AS nhs_number,
        GenderOfPatient AS sex,
        YearOfBirth AS yob,
        PracticeCode AS gpp_code,
        RegistrationDate AS reg_date,
        DeRegistrationDate AS dereg_date,
        DateOfDeath AS dod,
        Patient_LSOA AS lsoa
    FROM 
        [Warehouse_LDMPC].[dbo].[TPP_Patients]
  )
  SELECT * FROM emis
  UNION
  SELECT * FROM systmone;'
)

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

allc <- all_carers %>%
  filter(!is.na(nhs_number)) %>%
  transmute(
    nhs_number,
    gp_date = as_date(gp_date),
    carer = !str_detect(read_code, '918f')
  ) %>%
  left_join(patient_info3, by = 'nhs_number') %>%
  left_join(patients, by = 'nhs_number') %>%
  mutate(age = year(gp_date) - yob)

# test
if (!file.exists('patient_score2.RDS')) {
  combined_df <- cambridge %>%
    inner_join(patients, by = 'nhs_number') %>%
    left_join(
      cambridge_link %>% 
        distinct(link, .keep_all = TRUE) %>%
        select(link, period, n_required),
      by = 'link'
    ) %>% 
    mutate(link = str_replace(str_sub(link, 1, 3), 'ANX', 'DEP'))
  
  dates <- seq(ymd('2016-01-01'), ymd('2021-12-31'), by = '1 month')
  
  patient_score <- lapply(
    dates, 
    calculate_morbidity_score, 
    patient_cambridge_df = combined_df
  ) %>%
    bind_rows()
  
  saveRDS(patient_score, 'patient_score2.RDS')
  
  rm(dates, combined_df)
} else patient_score <- readRDS('patient_score2.RDS')

if (file.exists('patient_full.RDS')) {
  patient_full <- readRDS('patient_full.RDS')
} else {
  patient_full <- patients %>%
    filter(!is.na(reg_date)) %>%
    mutate(
      min_date = pmax(round_date(reg_date, 'month'), ymd('2016-01-01')),
      max_date = coalesce(round_date(dereg_date, 'month'), ymd('2021-12-01'))
    ) %>%
    filter(min_date <= max_date) %>%
    group_by(nhs_number) %>%
    mutate(cambridge_date = map2(min_date, max_date, seq, by = 'month')) %>%
    unnest(cambridge_date) %>%
    ungroup()
  
  patient_full <- patient_full %>%
    arrange(desc(cambridge_date), desc(reg_date)) %>%
    distinct(cambridge_date, nhs_number, .keep_all = TRUE)
  
  saveRDS(patient_full, 'patient_full.RDS')
}

carer_dates <- carer_dates %>%
  filter(start_date <= end_date) %>%
  group_by(nhs_number) %>%
  mutate(cambridge_date = map2(start_date, end_date, seq, by = 'month')) %>%
  unnest(cambridge_date) %>%
  ungroup() %>%
  mutate(carer = TRUE) %>%
  filter(year(cambridge_date) < 2022)

patient_df <- patient_full %>%
  left_join(patient_score, by = c('nhs_number', 'cambridge_date')) %>%
  left_join(carer_dates, by = c('nhs_number', 'cambridge_date')) %>%
  filter(year(cambridge_date) < 2022) %>%
  mutate(
    score = coalesce(score, 0),
    mortality_score = coalesce(mortality_score, 0),
    nel_score = coalesce(nel_score, 0),
    avg_score = coalesce(avg_score, 0),
    carer = coalesce(carer, FALSE),
    age = year(cambridge_date) - yob
  )

# CMS Comparison (visual)
patient_df %>%
  group_by(cambridge_date, carer) %>%
  summarise(
    lcl = quantile(nel_score, 0.025),
    median = median(nel_score),
    ucl = quantile(nel_score, 0.975)
  ) %>%
  arrange(carer, cambridge_date) %>%
  ggplot() +
    geom_line(aes(cambridge_date, median, colour = carer)) +
    ylim(0, NA)

patient_df2 %>%
  group_by(sex, cambridge_date, carer) %>%
  summarise(
    lcl = quantile(score, 0.025),
    median = median(score),
    ucl = quantile(score, 0.975)
  ) %>%
  arrange(carer, cambridge_date) %>%
  ggplot() +
  geom_line(aes(cambridge_date, median, colour = carer)) +
  ylim(0, NA) +
  facet_wrap(~ sex)

# age_bands <- data.frame(
#   age_band = c('<50', '50-69', '70-79', '80-89', '90+'),
#   age_min = c(18, 50, 70, 80, 90),
#   age_max = c(50, 69, 79, 89, Inf),
#   age_id = 1
# )
# 
# age_bands <- data.frame(
#   age_band = c('18-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '90+'),
#   age_min = c(18, 30, 40, 50, 60, 70, 80, 90),
#   age_max = c(29, 39, 49, 59, 69, 79, 89, Inf),
#   age_id = 1
# )

age_date_grouped <- patient_df2 %>%
  mutate(
    age = year(cambridge_date) - yob,
    age_id = 1
  ) %>%
  filter(age >= 18) %>%
  # left_join(age_bands, by = 'age_id') %>%
  # filter(age >= age_min, age <= age_max) %>%
  # select(-age_id, -age_min, -age_max) %>%
  mutate(
    age_band = case_when(
      age < 30 ~ '18-29',
      age < 40 ~ '30-39',
      age < 50 ~ '40-49',
      age < 60 ~ '50-59',
      age < 70 ~ '60-69',
      age < 80 ~ '70-79',
      TRUE ~ '80+'
    ),
    age_min = pmin(pmax(floor(age / 10) * 10, 20), 80)
  ) %>%
  group_by(sex, age_band, cambridge_date, carer) %>%
  summarise(
    lcl = quantile(nel_score, 0.025),
    median = median(nel_score),
    ucl = quantile(nel_score, 0.975)
  )

gc()

age_date_grouped %>%
  # mutate(age_min = age_min + 10 * carer) %>%
  ggplot() +
  geom_line(aes(
    cambridge_date, 
    median, 
    colour = age_band,
    linetype = carer,
    group = interaction(age_band, carer)
  )) +
  ylim(0, NA) +
  facet_wrap(~ sex)
  
gc()

patient_df <- patient_df %>%
    left_join(patient_info3, by = 'nhs_number')

imd_2019 <- calculate_deprivation_ntile()

patient_df <- patient_df %>%
  left_join(imd_2019, by = 'lsoa')

# When did people get vaccines?
vaccine_dates <- tibble(
  vaccine_date = c(
    ymd('2020-12-08'),
    ymd('2020-12-08'),
    ymd('2021-01-18'),
    # ymd('2021-02-15'),
    ymd('2021-03-01'),
    ymd('2021-03-17'),
    ymd('2021-04-30'),
    ymd('2021-05-26'),
    ymd('2021-06-18')
  ),
  age_min = c(
    90,
    80,
    70,
    # 65,
    60,
    50,
    40,
    30,
    18
  )
)

# 8 December 2020	Residents in a care home for older adults and their carers; and all aged 80 and over	1 and part of 2	[44][45]
# Procedures set out on 9 and 14 January 2021	Frontline health and social care workers	Part of 2	[46][47]
# 18 January 2021	All aged 70 and over, and clinically extremely vulnerable individuals	3 and 4	[48]
# 15 February 2021	All aged 65 and over; and those aged 16 to 64 with underlying health conditions which put them at higher risk of serious disease and mortality	5 and 6	[49]
# 1 March 2021	All aged 60 and over	7	[50]
# 6 March 2021	All aged 56 and over	8 (age adjusted from 55)	[51]
# 17 March 2021	All aged 50 and over	9	[52]
# 13 April 2021	All aged 45 and over		[53]
# 26 April 2021	All aged 44 and over		[54]
# 27 April 2021	All aged 42 and over		[55]
# 30 April 2021	All aged 40 and over		[56]
# 13 May 2021	All aged 38 and over		[57]
# 18 May 2021	All aged 36 and over		[58]
# 20 May 2021	All aged 34 and over		[59]
# 22 May 2021	All aged 32 and over		[60]
# 26 May 2021	All aged 30 and over		[61]
# 8 June 2021	All aged 25 and over		[62]
# 15 June 2021	All aged 23 and over		[63]
# 16 June 2021	All aged 21 and over		[64]
# 18 June 2021	All adults (ie aged 18 and over)