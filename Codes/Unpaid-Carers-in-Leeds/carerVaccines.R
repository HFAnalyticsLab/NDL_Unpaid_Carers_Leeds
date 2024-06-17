# Vaccine effect on carer registrations
need_packages <- c(
  'config',
  'tidyverse',
  'lubridate',
  'readxl',
  'odbc',
  'tidyquant',
  'plotly'
)

installed <- need_packages %in% installed.packages()
if(length(need_packages[!installed]) > 0) install.packages(need_packages[!installed], type = 'binary')
lapply(need_packages, library, character.only = TRUE)

options(stringsAsFactors = FALSE)
options('dplyr.summarise.inform' = FALSE)

config <- get()

# Load custom functions
invisible(
  lapply(
    Sys.glob('user_defined_functions/*.R'),
    function(x) source(x)
  )
)

#---- Get carers ----
carer_codes <- read_csv('codes/carerReadCodes.csv')

all_carers <- get_query(
  str_replace_all(
    read_file('sql/getFullPrimaryFromReadV2.sql'),
    c(
      '<READCODEV2>' = paste0("'", paste(carer_codes$read_code_v2, collapse = "', '"), "'")
    )
  )
)

care_ended <- all_carers %>% 
  filter(str_detect(read_code, '918f')) %>% 
  distinct(nhs_number)

# all_carers <- all_carers %>% 
#   anti_join(care_ended, by = 'nhs_number')

patients <- get_query(
  'SELECT 
      Patient_ID AS nhs_number,
      Gender AS sex,
      Year_of_Birth AS yob,
      Practice_Code AS gpp_code,
      Registration_Date AS reg_date,
      Deregistration_Date AS dereg_date,
      Date_of_Death AS dod
  FROM 
      [Warehouse_IHSC].[dbo].[GP_Patient_Register];'
)

patient_info <- get_query(
  'SELECT 
       * 
   FROM 
       Warehouse_IHSC.dbo.GP_All 
   WHERE 
       Patient_LSOA IS NOT NULL
   ORDER BY
       Attendance_Date;'
)

# *Should* get min/max dates for any LSOA for each patient
#    may take a while to run...
patient_info2 <- patient_info %>%
  group_by(Patient_ID, Patient_LSOA) %>%
  filter(Attendance_Date %in% range(Attendance_Date)) %>%
  ungroup() %>%
  distinct(
    nhs_number = Patient_ID,
    date = Attendance_Date,
    lsoa = Patient_LSOA
  ) %>%
  mutate(
    date = ymd(date)
  )

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

patient_info3 <- patient_info2 %>%
  group_by(nhs_number) %>%
  summarise(
    lsoa = mode(lsoa)
  ) %>%
  ungroup()

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

allc2 <- allc %>%
  group_by(nhs_number) %>%
  filter(gp_date == min(gp_date)) %>%
  ungroup() %>%
  distinct()

# Monthly new carer registrations, by age band and sex
new_carers <- allc2 %>% 
  mutate(
    age_band = pmin(pmax(10 * floor(age / 10), 20), 90)
  )  %>% 
  count(gp_date = round_date(gp_date, 'month'), sex, age_band) %>%
  drop_na() %>%
  filter(gp_date > ymd('2015-12-01'))

# #---- Plot effect of vaccine drives (usual flu + covid) ----
all_plot <- new_carers %>%
  ggplot() +
  geom_point(aes(gp_date, n, colour = sex), size = 1, alpha = 0.5) +
  facet_wrap(~ age_band, scales = 'free_y') +
  xlim(ymd('2016-01-01'), ymd('2021-12-01')) +
  tidyquant::geom_ma(
    aes(gp_date, n, colour = sex),
    n = 1,
    linetype = 'solid',
    size = 1
  )

# # All manually defined but add some lines to highlight points of interest
for (d in c(2016, 2017, 2018, 2019, 2020, 2021)) {
  if (d == 2016) all_plot2 <- all_plot
  if (d < 2020) {
    all_plot2 <- all_plot2 +
      geom_vline(
        xintercept = ymd(paste0(d, '-09-01')),
        size = 0.5,
        linetype = 'dashed',
        colour = 'black'
      ) +
      geom_vline(
        xintercept = ymd(paste0(d, '-11-01')),
        size = 0.5,
        linetype = 'dashed',
        colour = 'black'
      )
  }
  if (d == 2021) {
    all_plot2 <- all_plot2 +
      geom_vline(xintercept = ymd(paste0(d, '-01-01')), size = 0.5, linetype = 'dashed', colour = 'black') +
      geom_vline(xintercept = ymd(paste0(d, '-03-01')), size = 0.5, linetype = 'dashed', colour = 'black')
  }
  if (d == 2020) {
    all_plot2 <- all_plot2 +
      geom_vline(xintercept = ymd(paste0(d, '-03-01')), size = 0.5, linetype = 'dashed', colour = 'black') +
      geom_vline(xintercept = ymd(paste0(d, '-05-01')), size = 0.5, linetype = 'dashed', colour = 'black')
  }
}

all_plot2

#---- Signal to noise ----
new_carers <- new_carers %>%
  mutate(
    signal = case_when(
      (age_band < 70) & (year(gp_date) < 2020) & between(month(gp_date), 9, 11) ~ TRUE,
      (age_band < 70) & (year(gp_date) == 2021) & between(month(gp_date), 1, 3) ~ TRUE,
      (age_band >= 60) & (year(gp_date) == 2020) & between(month(gp_date), 3, 5) ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  group_by(age_band) %>%
  mutate(
    n_min = 0,
    n_max = max(n)
  ) %>%
  ungroup() %>%
  mutate(n_max = if_else(!signal, 0L, n_max))

new_carers %>%
  ggplot() + 
    geom_ribbon(
      aes(gp_date, ymin = n_min, ymax = n_max), 
      # data = filter(new_carers, signal),
      alpha = 0.2
    ) +
    geom_line(aes(gp_date, n, colour = sex), size = 1) +
    facet_wrap(~ age_band, scales = 'free_y')

poi <- new_carers %>%
  filter(signal) %>%
  group_by(age_band, sex) %>%
  mutate(period = pmax(floor((row_number() - 1) / 3), 0)) %>%
  # group_by(age_band, sex, period) %>%
  # filter(gp_date == max(gp_date)) %>%
  ungroup() %>%
  select(gp_date, sex, age_band, period)

new_carers2 <- new_carers %>%
  left_join(poi, by = c('gp_date', 'age_band', 'sex')) %>%
  fill(period, .direction = 'up') 

new_carers3 <- new_carers2 %>%
  left_join(
    new_carers2 %>%
      group_by(sex, age_band, period, signal) %>%
      summarise(
        max = max(n),
        ucl = max + sd(n)
      ) %>%
      group_by(period, age_band, sex) %>%
      summarise(
        snr = max[signal] / max[!signal],
        snr_lcl = max[signal] / ucl[!signal]
      ) %>%
      ungroup(),
    by = c('period', 'sex', 'age_band')
  )

carer_plot <- new_carers3 %>%
  # filter(sex == 'F') %>%
  ggplot(aes(text = round(snr, 2), text2 = round(snr_lcl, 2))) +
  geom_ribbon(
    aes(gp_date, ymin = n_min, ymax = n_max),
    alpha = 0.2
  ) +
  geom_line(aes(gp_date, n, colour = sex), size = 1) +
  facet_wrap(~ age_band, scales = 'free_y')

# ggplotly(carer_plot, tooltip = c('text', 'text2'))

carer_plot

#--- Compare demographics ----
carer_info <- allc2 %>% 
  transmute(
    nhs_number,
    gp_date,
    carer,
    lsoa, 
    sex,
    yob,
    age,
    age_band = pmin(pmax(10 * floor(age / 10), 20), 90),
    date = round_date(gp_date, 'month')
  ) %>%
  inner_join(new_carers2, by = c('date' = 'gp_date', 'sex', 'age_band')) %>%
  filter(!is.na(lsoa))

imd_2019 <- calculate_deprivation_ntile()

imd_range_df <- imd_2019 %>% 
  group_by(deprivation_decile) %>% 
  summarise(
    min_score = min(combined_score), 
    max_score = max(combined_score)
  )

# How many people register as carers per month?
carer_info %>%
  group_by(signal, sex) %>%
  summarise(
    n = n(),
    min_date = min(date),
    max_date = max(date),
    months = n_distinct(date),
    avg = n / months
  )

get_imd_from_score <- function(score, imd = imd_range_df) {
  imd %>% 
    filter(score >= min_score, score < max_score) %>%
    pull(deprivation_decile)
}


carer_imd <- carer_info %>%
  left_join(imd_2019, by = 'lsoa') %>%
  drop_na() %>%
  group_by(period, signal, age_band, sex) %>%
  summarise(mean_score = mean(combined_score, na.rm = TRUE)) %>%
  ungroup()

carer_imd <- carer_imd %>%
  rowwise() %>%
  mutate(mean_imd = get_imd_from_score(mean_score)) %>%
  ungroup()

# carer_imd %>%
#   mutate(period = period + if_else(signal, 0.5, 0)) %>%
#   ggplot() +
#     geom_line(aes(period, mean_imd, colour = sex), size = 1) +
#     facet_wrap(~ age_band)

carer_imd %>%
  group_by(period, age_band, sex) %>%
  summarise(
    score_diff = mean_score[signal] - mean_score[!signal],
    decile_diff = mean_imd[signal] - mean_imd[!signal]
  ) %>%
  ungroup() %>%
  count(age_band, sex, decile_diff) %>%
  ggplot() +
    geom_raster(aes(age_band, decile_diff, fill = n)) +
    scale_fill_gradient(high = 'red', low = 'blue')

# Monthly IMD
carer_date <- carer_info %>%
  left_join(imd_2019, by = 'lsoa') %>%
  drop_na() %>%
  group_by(age_band, sex, date) %>%
  summarise(mean_score = mean(combined_score, na.rm = TRUE)) %>%
  ungroup()

carer_date <- carer_date %>%
  rowwise() %>%
  mutate(mean_imd = get_imd_from_score(mean_score)) %>%
  ungroup()

carer_date <- carer_date %>%
  left_join(
    select(new_carers2, date = gp_date, sex, age_band, signal, period),
    by = c('date', 'sex', 'age_band')
  )

# Change in
carer_date %>%
  filter(sex == 'F') %>%
  ggplot() +
  geom_point(aes(date, mean_score, size = signal)) +
  geom_line(
    aes(date, mean_score, colour = imd_shift),
    size = 0.2,
    data = carer_date %>%
      filter(sex == 'F') %>%
      group_by(age_band) %>%
      mutate(
        imd_shift = lead(mean_imd) - (mean_imd),
        imd_shift = pmin(pmax(imd_shift, -4), 4)
      ) %>%
      ungroup()
  ) +
  facet_wrap(~ age_band) +
  scale_color_gradient2(
    midpoint = 0,
    low = 'red',
    mid = 'white',
    high = 'blue',
    limits = c(-4, 4)
  ) +
  scale_size_manual(values = c(1, 3))

carer_date %>%
  filter(sex == 'M') %>%
  ggplot() +
  geom_point(aes(date, mean_score, size = signal)) +
  geom_line(
    aes(date, mean_score, colour = imd_shift),
    size = 0.2,
    data = carer_date %>%
      filter(sex == 'M') %>%
      group_by(age_band) %>%
      mutate(
        imd_shift = lead(mean_imd) - (mean_imd),
        imd_shift = pmin(pmax(imd_shift, -4), 4)
      ) %>%
      ungroup()
  ) +
  facet_wrap(~ age_band) +
  scale_color_gradient2(
    midpoint = 0,
    low = 'red',
    mid = 'white',
    high = 'blue',
    limits = c(-4, 4)
  ) +
  scale_size_manual(values = c(1, 3))

# Compare by sex IMD decile shift distributions (signal vs non-signal)
# No real difference
carer_date %>%
  group_by(age_band, sex) %>%
  mutate(
    imd_shift = lead(mean_imd) - mean_imd,
    imd_score_shift = lead(mean_score) - mean_score
  ) %>%
  filter(!is.na(imd_shift)) %>%
  ungroup() %>%
  count(signal, sex, imd_shift) %>%
  group_by(signal) %>%
  mutate(p = n / sum(n)) %>%
  ggplot() +
    geom_line(aes(imd_shift, p, colour = signal, group = signal)) +
    facet_wrap(~ sex)

# Compare by sex IMD score shift distributions (signal vs non-signal)
# No difference female, slight shift to less deprived male but very small
carer_date %>%
  group_by(age_band, sex) %>%
  mutate(
    imd_shift = lead(mean_imd) - mean_imd,
    imd_score_shift = lead(mean_score) - mean_score
  ) %>%
  filter(!is.na(imd_shift)) %>%
  ungroup() %>%
  ggplot(
    aes(x = imd_score_shift, fill = signal)
  ) +
  geom_histogram(
    aes(y = ..density..),
    alpha = 0.4,
    position = 'identity',
    binwidth = 5
  ) +
  facet_wrap(~ sex)

carer_info %>%
  left_join(imd_2019, by = 'lsoa') %>%
  drop_na() %>%
  ggplot(
    aes(x = combined_score, fill = signal)
  ) +
  geom_histogram(
    aes(y = ..density..),
    alpha = 0.4,
    position = 'identity',
    binwidth = 5
  ) +
  facet_wrap(~ sex)

# What's the difference in IMD scores between folks who registered during 
#   vaccine drives and those who registered in "normal" times?
carer_info %>%
  left_join(imd_2019, by = 'lsoa') %>%
  drop_na() %>% 
  group_by(sex = age_band, signal) %>%
  mutate(mean_signal = mean(combined_score)) %>%
  group_by(deprivation_decile) %>%
  mutate(min_score = min(combined_score)) %>%
  ungroup() %>%
  ggplot(
    aes(x = combined_score, fill = signal, colour = signal)
  ) + 
  annotate("text", x = 46, y = 0.005, label = "IMD 1", angle=90, size = 4) +
  annotate("text", x = 38, y = 0.005, label = "IMD 2", angle=90, size = 4) +
  annotate("text", x = 30, y = 0.005, label = "IMD 3", angle=90, size = 4) +
  annotate("text", x = 24, y = 0.005, label = "IMD 4", angle=90, size = 4) +
  annotate("text", x = 19.5, y = 0.005, label = "IMD 5", angle=90, size = 4) +
  annotate("text", x = 15.5, y = 0.005, label = "IMD 6", angle=90, size = 4) +
  annotate("text", x = 12, y = 0.005, label = "IMD 7", angle=90, size = 4) +
  annotate("text", x = 9.5, y = 0.005, label = "IMD 8", angle=90, size = 4) +
  annotate("text", x = 7, y = 0.005, label = "IMD 9", angle=90, size = 4) +
  annotate("text", x = 3.5, y = 0.005, label = "IMD 10", angle=90, size = 4) +
  geom_vline(aes(xintercept = min_score), alpha = 0.2) +
  geom_density(alpha = 0.4, kernel = 'rectangular') + 
  geom_vline(aes(xintercept = mean_signal, colour = signal), size = 1.5) +
  facet_wrap(~ sex) +
  xlab('IMD Combined Score') +
  ylab('Density')

# Wilcox test, testing whether male carer registered during vaccine drives are
#   more likely to be from areas with lower deprivation scores
# Found to be statistically significant (p < .0001) - they do come from lower
#   IMD score areas
wilcox.test(
  carer_info %>%
    left_join(imd_2019, by = 'lsoa') %>%
    drop_na() %>% 
    filter(sex == 'M', signal) %>% 
    pull(combined_score), 
  carer_info %>%
    left_join(imd_2019, by = 'lsoa') %>%
    drop_na() %>% 
    filter(sex == 'M', !signal) %>% 
    pull(combined_score), 
  alternative = 'l'
)

# effect size
rstatix::wilcox_effsize(
  carer_info %>%
    left_join(imd_2019, by = 'lsoa') %>%
    drop_na() %>%
    filter(sex == 'M') %>%
    mutate(signal = factor(if_else(signal, 'A', 'B'))),
  formula = combined_score ~ signal,
  alternative = 'l'
)

# Wilcox test, testing whether female carer registered during vaccine drives are
#   more likely to be from areas with lower deprivation scores
# Found to be "just" statistically significant (p = 0.01) - they do come from lower
#   IMD score areas
wilcox.test(
  carer_info %>%
    left_join(imd_2019, by = 'lsoa') %>%
    drop_na() %>% 
    filter(sex == 'F', signal) %>% 
    pull(combined_score), 
  carer_info %>%
    left_join(imd_2019, by = 'lsoa') %>%
    drop_na() %>% 
    filter(sex == 'F', !signal) %>% 
    pull(combined_score), 
  alternative = 'l'
)

rstatix::wilcox_effsize(
  carer_info %>%
    left_join(imd_2019, by = 'lsoa') %>%
    drop_na() %>%
    filter(sex == 'F') %>%
    mutate(signal = factor(if_else(signal, 'A', 'B'))),
  formula = combined_score ~ signal,
  alternative = 'l'
)

# Get cambridge scores
patient_df <- readRDS('patient_df.RDS')

carer_cms <- carer_info %>%
  left_join(patient_df, by = c('nhs_number', 'date' = 'cambridge_date')) #%>%
  # drop_na()

# carer_cms %>%
#   ggplot() +
#     geom_density(aes(nel_score, fill = signal), alpha = 0.4) +
#     geom_vline(aes(xintercept = mean(nel_score), colour = signal))

carer_cms %>%
  filter(year(date) < 2022, age_band < 60) %>%
  ggplot(aes(nel_score, fill = signal, colour = signal)) +
    geom_density(alpha = 0.4) +
    stat_summary(
      aes(xintercept = ..x.., y = 0), 
      fun = mean, 
      geom = "vline", 
      orientation = "y",
      size = 1
    ) +
    facet_wrap(~ age_band + year(date))

# carer_cms <- carer_info %>%
#   left_join(patient_df, by = c('nhs_number', 'date' = 'cambridge_date')) %>%
#   drop_na() %>%
#   group_by(age_band, sex = sex.x, date) %>%
#   summarise(mean_score = mean(nel_score, na.rm = TRUE)) %>%
#   ungroup() 
# 
# carer_cms <- carer_cms %>%
#   left_join(
#     select(new_carers2, date = gp_date, sex, age_band, signal, period),
#     by = c('date', 'sex', 'age_band')
#   )

