ethnicity_codes <- read_csv('codes/ethnicityReadCodesCTV3.csv') %>%
  rowwise() %>%
  mutate(
    category = case_when(
      Grouping_6 == 1 ~ 'White',
      Grouping_6 == 2 ~ 'Mixed',
      Grouping_6 == 4 ~ 'Black',
      Grouping_6 == 3 ~ 'South Asian',
      Grouping_6 == 5 ~ 'Other',
      Grouping_6 == 6 ~ 'Not Stated'
    ),
    group = switch(
      Grouping_16,
      'White British',
      'White Irish',
      'Other White',
      'White and Caribbean',
      'White and African',
      'White and Asian',
      'Other mixed',
      'Indian',
      'Pakistani',
      'Bangladeshi',
      'Other South Asian',
      'Caribbean',
      'African',
      'Other Black',
      'Chinese',
      'All other ethnic groups',
      'Not stated'
    )
  ) %>%
  ungroup()

ethnicity <- get_query(
  str_replace_all(
    read_file('sql/getFullPrimaryFromReadV3.sql'),
    c(
      '<READCODECTV3>' = paste0("'", paste(ethnicity_codes$Code, collapse = "', '"), "'")
    )
  )
) %>%
  mutate(gp_date = as_date(gp_date)) %>%
  distinct()

ethnicity <- ethnicity %>%
  left_join(ethnicity_codes, by = c('read_code_ctv3' = 'Code'))

absyc <- patients %>% 
  mutate(year = year(cambridge_date)) %>%
  arrange(-carer) %>% 
  distinct(nhs_number, year, .keep_all = TRUE) %>% 
  mutate(
    age_band = pmin(pmax(10 * floor(age / 10), 20), 90)
  ) %>%
  distinct(
    nhs_number, 
    age_band, 
    age,
    sex, 
    year, 
    carer, 
    language = language_simple, 
    imd = deprivation_decile, 
    ethnicity = group
  )

for (y in 2016:2021) {
  if (y == 2016) models <- list()
  models[[y - 2015]] <- glm(
    carer ~ age_band + sex + language + imd + ethnicity,
    data = absyc %>% filter(year == y) %>% mutate(
      language = if_else(language == 'Unknown', 'English', language),
      ethnicity = relevel(factor(ethnicity), ref = 'White British')
    ),
    family = binomial(link = 'logit')
  )
}

for (y in 2016:2021) {
  if (y == 2016) coef_list <- list()
  coef_list[[y - 2015]] <- exp(cbind(coef(models[[y - 2015]]), confint(models[[y - 2015]]))) 
}

lapply(coef_list, function(x) {
  as.data.frame.table(x) %>%
    transmute(
      variable = Var1,
      level = case_when(
        Var2 == '' ~ 'odds_ratio',
        Var2 == '2.5 %' ~ 'lcl',
        Var2 == '97.5 %' ~ 'ucl'
      ),
      odds_ratio = Freq
    ) %>%
    pivot_wider(names_from = level, values_from = odds_ratio)
}) %>%
  bind_rows() -> odds

odds <- odds %>%
  mutate(
    year = rep(2016:2021, each = 20),
    variable_name = variable,
    variable = paste(variable, year)
  ) %>%
  group_by(variable_name) %>%
  mutate(
    not_significant = (mean(lcl) < 1) & (mean(ucl) > 1)
  ) %>%
  ungroup()

background <- odds %>%
  filter(variable_name != '(Intercept)') %>%
  group_by(variable_name) %>%
  summarise(
    ymin = pmin(1, min(lcl)),
    ymax = pmax(1, max(ucl)),
    xmin = 2016,
    xmax = 2021,
    significant = !not_significant
  ) %>%
  ungroup()

oddsPlot <- odds %>% 
  mutate(significant = !not_significant) %>%
  filter(variable_name != '(Intercept)') %>%
  ggplot() +
    geom_rect(
      aes(
        xmin = xmin, 
        xmax = xmax, 
        ymin = ymin, 
        ymax = ymax, 
        fill = significant
      ), 
      alpha = 0.1, 
      data = background
    ) +
    geom_hline(yintercept = 1, linetype = 'dashed', size = 1, colour = 'red') +
    geom_linerange(aes(year, ymin = lcl, ymax = ucl, colour = variable_name, fill = significant), size = 1) +
    geom_smooth(aes(year, odds_ratio, colour = variable_name), se = F, method = 'lm') +
    theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
    facet_wrap(~ variable_name, scales = 'free_y') +
    xlab('') +
    ylab('Odds Ratio') +
    scale_colour_discrete(name = 'Variable', guide = 'none') +
    scale_fill_discrete(name = 'Significant')

save(oddsPlot, file = '../project-3-scrolly/data/oddsPlot.RData')
