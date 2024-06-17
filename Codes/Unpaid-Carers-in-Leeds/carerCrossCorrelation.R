need_packages <- c(
  'config',
  'tidyverse',
  'lubridate',
  'readxl',
  'odbc'
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

#---- Load patients ----
patient_df <- readRDS('patient_df.RDS') %>%
  filter(age > 17)

cambridge_dates <- patient_df %>%
  distinct(cambridge_date) %>%
  mutate(cambridge_date = as_date(cambridge_date)) %>%
  pull(cambridge_date)

#---- Compare nel score distribution of carers/non-carer by age-band ----
date <- cambridge_dates[1]
cat(as.character(date), '    ')

age_band_width <- 10

test_patients <- patient_df %>% 
  filter(cambridge_date == date) %>%
  mutate(
    age_band = pmin(pmax(age_band_width * floor(age / age_band_width), 20), 90)
  ) %>%
  filter(age < 100)

# Create dummy data frame with all age-band/carer/nel score/nel bin combinations
age_bands <- tibble(
  age_band = seq(20, 90, age_band_width),
  id = 1
)

bin_width <- test_patients %>%
  group_by(carer, age_band) %>%
  summarise(
    h = round(2 * IQR(nel_score) * (n() ^ (-1 / 3)))
  ) %>%
  ungroup() %>%
  summarise(h = ceiling(mean(h))) %>%
  pull(h)

breaks <- tibble(
  bin = seq(0, ceiling(max(test_patients$nel_score)), bin_width),
  id = 1
)

bins <- tibble(
  nel = seq(0, ceiling(max(test_patients$nel_score))),
  id = 1
) %>%
  left_join(breaks, by = 'id') %>%
  filter(nel >= bin, nel < (bin + bin_width))

hist_df <- distinct_expand(test_patients, carer) %>%
  full_join(age_bands, by = 'id') %>%
  full_join(bins, by = 'id') %>%
  select(-id)

#---- Shift carer ages from (-a, a) years & compare nel distributions ----
shift_range <- seq(-50, 50)

for (a in shift_range) {
  cat(a, '\n')
  
  if (a == min(shift_range)) {
    ks <- tibble()
  }
  
  tp <- test_patients %>%
    transmute(
      carer, 
      nel = nel_score, 
      age = age + if_else(carer, as.numeric(a), 0),
      age_band = pmin(pmax(age_band_width * floor(age / age_band_width), 20), 90)
    ) %>%
    filter(age < 100)
  
  histogram_df <- tp %>%
    mutate(
      nel = round(nel),
      n = 1
    ) %>%
    right_join(hist_df, by = c('carer', 'age_band', 'nel')) %>%
    group_by(age_band, carer, bin) %>%
    summarise(n = sum(n, na.rm = TRUE)) %>%
    group_by(age_band, carer) %>%
    mutate(
      perct = n / sum(n) / bin_width,
      lcl = (n - sqrt(n) / 2) / sum(n) / bin_width,
      ucl = (n + sqrt(n) / 2) / sum(n) / bin_width,
    ) %>%
    ungroup()
  
  ks <- ks %>% bind_rows(
    histogram_df %>% 
      group_by(age_band) %>%
      arrange(bin) %>%
      summarise(
        n_carer = sum(n[carer]),
        n_valid = n_carer > 100,
        chi_square = chi_square_by_group(perct, carer),
        chi_lcl = chi_square_by_group(lcl, carer),
        chi_ucl = chi_square_by_group(ucl, carer)
      ) %>%
      ungroup() %>%
      mutate(offset = a)
  )
}

# Calculate best age-lag for carers
best_offset <- ks %>%
  group_by(offset) %>%
  summarise(
    mean_chi = sum(chi_square) / sum(n_valid),
    mean_lcl = sum(chi_lcl) / sum(n_valid),
    mean_ucl = sum(chi_ucl) / sum(n_valid)
  ) %>%
  ungroup() %>%
  filter(mean_chi == min(mean_chi))

plot_nel_comparison(test_patients, ymd('2021-12-01'), 0)
plot_nel_comparison(test_patients, ymd('2021-12-01'), 12)

nel_plots <- list()

for (s in 0:30) {
  nel_plots[[s + 1]] <- plot_nel_comparison(test_patients, ymd('2021-12-01'), s, age_band_filter = 60)
}

save(nel_plots, file = 'nelPlots.RData')

# Plot the mean chi-square per age lag
ks %>%
  group_by(offset) %>%
  summarise(
    mean_chi = sum(chi_square) / sum(n_valid),
    mean_lcl = sum(chi_lcl) / sum(n_valid),
    mean_ucl = sum(chi_ucl) / sum(n_valid)
  ) %>%
  ungroup() %>% 
  ggplot() + 
    geom_line(aes(offset, mean_chi)) +
    geom_errorbar(aes(offset, ymin = mean_lcl, ymax = mean_ucl)) +
    ylim(0, NA) +
    scale_y_log10()

# If multiple lags are optimal (overlapping CIs), calculate mean "best" lag
mean_offset <- ks %>%
  group_by(offset) %>%
  summarise(
    mean_chi = sum(chi_square) / sum(n_valid),
    mean_lcl = sum(chi_lcl) / sum(n_valid),
    mean_ucl = sum(chi_ucl) / sum(n_valid)
  ) %>%
  ungroup() %>%
  filter(
    mean_lcl <= best_offset$mean_ucl
  ) %>% 
  summarise(mean_offset = mean(offset))
