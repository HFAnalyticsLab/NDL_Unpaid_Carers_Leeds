deprivation_density <- carer_info %>%
  drop_na() %>% 
  filter(ymd(date) >= '2020-01-01') %>%
  group_by(signal) %>%
  mutate(mean_signal = mean(combined_score)) %>%
  group_by(deprivation_decile) %>%
  mutate(min_score = min(combined_score)) %>%
  ungroup() %>%
  ggplot(
    aes(x = combined_score, fill = signal, colour = signal)
  ) + 
  geom_vline(aes(xintercept = min_score), alpha = 0.2) +
  geom_density(alpha = 0.4, kernel = 'rectangular') + 
  geom_vline(aes(xintercept = mean_signal, colour = signal), size = 1.5) +
  xlab('IMD Combined Score') +
  ylab('Density')

deprivationTotalPlotCovid <- ggplotly(deprivation_density) %>%
  layout(
    title = element_blank(),
    font = list(family = 'Lato'),
    margin = list(t=50),
    hoverlabel = list(bgcolor = 'whitesmoke', color = 'DarkGray')) %>% 
  config(displaylogo = F, showSendToCloud = F, displayModeBar = F)

deprivationTotalPlotCovid <- deprivationTotalPlotCovid %>%
  add_annotations(
    x = 3.5, y = 0.005, text = "IMD 10", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 7, y = 0.005, text = "IMD 9", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 9.5, y = 0.005, text = "IMD 8", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 12, y = 0.005, text = "IMD 7", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 15.5, y = 0.005, text = "IMD 6", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 19.5, y = 0.005, text = "IMD 5", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 24, y = 0.005, text = "IMD 4", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 30, y = 0.005, text = "IMD 3", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 38, y = 0.005, text = "IMD 2", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 46, y = 0.005, text = "IMD 1", textangle = -90, showarrow = FALSE
  )

# Making legends nicer
df <- data.frame(
  id = seq_along(deprivationTotalPlotCovid$x$data), 
  legend_entries = unlist(lapply(deprivationTotalPlotCovid$x$data, `[[`, "name"))
)
df$legend_group <- gsub("^\\((.*?),\\d+\\)", "\\1", df$legend_entries)
df$is_first <- !duplicated(df$legend_group)

for (i in df$id) {
  is_first <- df$is_first[[i]]
  deprivationTotalPlotCovid$x$data[[i]]$name <- df$legend_group[[i]]
  deprivationTotalPlotCovid$x$data[[i]]$legendgroup <- deprivationTotalPlotCovid$x$data[[i]]$name
  if (!is_first) deprivationTotalPlotCovid$x$data[[i]]$showlegend <- FALSE
}

deprivation_male_density <- carer_info %>%
  drop_na() %>% 
  filter(ymd(date) >= '2020-01-01') %>%
  filter(sex == 'M') %>%
  group_by(sex, signal) %>%
  mutate(mean_signal = mean(combined_score)) %>%
  group_by(deprivation_decile) %>%
  mutate(min_score = min(combined_score)) %>%
  ungroup() %>%
  ggplot(
    aes(x = combined_score, fill = signal, colour = signal)
  ) + 
  geom_vline(aes(xintercept = min_score), alpha = 0.2) +
  geom_density(alpha = 0.4, kernel = 'rectangular') + 
  geom_vline(aes(xintercept = mean_signal, colour = signal), size = 1.5) +
  facet_wrap(~ sex) +
  xlab('IMD Combined Score') +
  ylab('Density')

deprivationPlotCovid <- ggplotly(deprivation_male_density) %>%
  layout(
    title = element_blank(),
    font = list(family = 'Lato'),
    margin = list(t=50),
    hoverlabel = list(bgcolor = 'whitesmoke', color = 'DarkGray')) %>% 
  config(displaylogo = F, showSendToCloud = F, displayModeBar = F)

deprivationPlotCovid <- deprivationPlotCovid %>%
  add_annotations(
    x = 3.5, y = 0.005, text = "IMD 10", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 7, y = 0.005, text = "IMD 9", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 9.5, y = 0.005, text = "IMD 8", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 12, y = 0.005, text = "IMD 7", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 15.5, y = 0.005, text = "IMD 6", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 19.5, y = 0.005, text = "IMD 5", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 24, y = 0.005, text = "IMD 4", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 30, y = 0.005, text = "IMD 3", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 38, y = 0.005, text = "IMD 2", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 46, y = 0.005, text = "IMD 1", textangle = -90, showarrow = FALSE
  )

deprivation_female_density <- carer_info %>%
  drop_na() %>% 
  filter(ymd(date) >= '2020-01-01') %>%
  filter(sex == 'F') %>%
  group_by(sex, signal) %>%
  mutate(mean_signal = mean(combined_score)) %>%
  group_by(deprivation_decile) %>%
  mutate(min_score = min(combined_score)) %>%
  ungroup() %>%
  ggplot(
    aes(x = combined_score, fill = signal, colour = signal)
  ) +
  geom_vline(aes(xintercept = min_score), alpha = 0.2) +
  geom_density(alpha = 0.4, kernel = 'rectangular') + 
  geom_vline(aes(xintercept = mean_signal, colour = signal), size = 1.5) +
  facet_wrap(~ sex) +
  xlab('IMD Combined Score') +
  ylab('Density')

deprivationPlotFCovid <- ggplotly(deprivation_female_density) %>%
  layout(
    title = element_blank(),
    font = list(family = 'Lato'),
    margin = list(t=50),
    hoverlabel = list(bgcolor = 'whitesmoke', color = 'DarkGray')) %>% 
  config(displaylogo = F, showSendToCloud = F, displayModeBar = F)

deprivationPlotFCovid <- deprivationPlotFCovid %>%
  add_annotations(
    x = 3.5, y = 0.005, text = "IMD 10", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 7, y = 0.005, text = "IMD 9", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 9.5, y = 0.005, text = "IMD 8", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 12, y = 0.005, text = "IMD 7", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 15.5, y = 0.005, text = "IMD 6", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 19.5, y = 0.005, text = "IMD 5", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 24, y = 0.005, text = "IMD 4", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 30, y = 0.005, text = "IMD 3", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 38, y = 0.005, text = "IMD 2", textangle = -90, showarrow = FALSE
  ) %>%
  add_annotations(
    x = 46, y = 0.005, text = "IMD 1", textangle = -90, showarrow = FALSE
  )

deprivationPlotCovid <- subplot(deprivationPlotCovid, deprivationPlotFCovid)

# Making legends nicer
df <- data.frame(
  id = seq_along(deprivationPlotCovid$x$data), 
  legend_entries = unlist(lapply(deprivationPlotCovid$x$data, `[[`, "name"))
)
df$legend_group <- gsub("^\\((.*?),\\d+\\)", "\\1", df$legend_entries)
df$is_first <- !duplicated(df$legend_group)

for (i in df$id) {
  is_first <- df$is_first[[i]]
  deprivationPlotCovid$x$data[[i]]$name <- df$legend_group[[i]]
  deprivationPlotCovid$x$data[[i]]$legendgroup <- deprivationPlotCovid$x$data[[i]]$name
  if (!is_first) deprivationPlotCovid$x$data[[i]]$showlegend <- FALSE
}

# Age Band
deprivation_age_band_density <- carer_info %>%
  drop_na() %>% 
  filter(ymd(date) >= '2020-01-01') %>%
  group_by(age_band, signal) %>%
  mutate(mean_signal = mean(combined_score)) %>%
  group_by(deprivation_decile) %>%
  mutate(min_score = min(combined_score)) %>%
  ungroup() %>%
  ggplot(
    aes(x = combined_score, fill = signal, colour = signal)
  ) + 
  geom_vline(aes(xintercept = min_score), alpha = 0.2) +
  geom_density(alpha = 0.4, kernel = 'rectangular') + 
  geom_vline(aes(xintercept = mean_signal, colour = signal), size = 1.5) +
  xlab('IMD Combined Score') +
  ylab('Density') + 
  facet_wrap(~ age_band)

deprivationAgeBandPlotCovid <- ggplotly(deprivation_age_band_density) %>%
  layout(
    title = element_blank(),
    font = list(family = 'Lato'),
    margin = list(t=50),
    hoverlabel = list(bgcolor = 'whitesmoke', color = 'DarkGray')) %>% 
  config(displaylogo = F, showSendToCloud = F, displayModeBar = F)

# Making legends nicer
df <- data.frame(
  id = seq_along(deprivationAgeBandPlotCovid$x$data), 
  legend_entries = unlist(lapply(deprivationAgeBandPlotCovid$x$data, `[[`, "name"))
)
df$legend_group <- gsub("^\\((.*?),\\d+\\)", "\\1", df$legend_entries)
df$is_first <- !duplicated(df$legend_group)

for (i in df$id) {
  is_first <- df$is_first[[i]]
  deprivationAgeBandPlotCovid$x$data[[i]]$name <- df$legend_group[[i]]
  deprivationAgeBandPlotCovid$x$data[[i]]$legendgroup <- deprivationAgeBandPlotCovid$x$data[[i]]$name
  if (!is_first) deprivationAgeBandPlotCovid$x$data[[i]]$showlegend <- FALSE
}