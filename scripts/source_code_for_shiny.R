library(ggplot2)
library(ggthemes)
library(dplyr)
library(ggrepel)
library(tools)
library(readxl)
library(tidyverse)
library(knitr)
library(shiny)
library(shinyjs)
library(ggvis)
library(plotly)
library(metathis)
library(formattable)
library(lubridate)
# devtools::install_github("statistiekcbs/scrollytell")
library(scrollytell)
library(here)
library(shinycssloaders)
library(leaflet)
library(sf)
library(geographr)

options(scipen = 999)
options(warn = -1)

theme_set(theme_minimal())

data <- readRDS(here('data/new_carers3.RDS')) %>%
  filter(age_band < 70, year(gp_date) < 2020) %>%
  group_by(gp_date, age_band) %>%
  mutate(
    n = sum(n)
  ) %>%
  group_by(age_band) %>%
  mutate(n_max = if_else(n_max == 0, n_max, max(n))) %>%
  ungroup() %>%
  mutate(
    sex = 'MF',
    reveal = group_indices(., year(gp_date))
  )

carer_info <- readRDS(here('data/carer_info.rds')) %>% 
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

carer_registrations <- bind_rows(
  readRDS(here('data/overallCarerRegistrations.rds')) %>%
    mutate(group = 'total'),
  readRDS(here('data/sexCarerRegistrations.rds')) %>%
    mutate(group = 'sex'),
  readRDS(here('data/ageBandCarerRegistrations.rds')) %>%
    mutate(group = 'age_band'),
  readRDS(here('data/imdCarerRegistrations.rds')) %>%
    mutate(group = 'imd'),
  readRDS(here('data/ageBandImdCarerRegistrations.rds')) %>%
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

carer_lsoa <- readRDS(here('data/lsoaCarerRegistrations.rds')) %>%
  mutate(group = 'lsoa')

carer_language <- readRDS(here('data/carerLanguage.rds'))

carer_period <- readRDS(here('data/carerPeriod.rds'))

### FUNCTIONS & TEXT

longdiv <- function(...){
  div(
    ...,
    class = "container",
    style = "height:100vh"
  )
}

render_text <- function(num){
  
  div(
    text(num), class = "text"
  )
  
}

null_na <- function(x) {
  if_else(is.null(x), NA_character_, x)
}

text <- function(num){
  p(
    switch(num,
           text1,
           text2,
           text3,
           text4,
           text5,
           text6
    )
  )
}

intro_text <- HTML(
  "<span style='font-size:20px'> How much to we know about unpaid carers in Leeds? </span>
              <br><br> 
  <p>The Parliamentary Family Resources Survey (2021) estimated that in 2019/20 
  around 7% of the UK population were providing unpaid care, although their 
  definition is ‘informal care’. Similarly, the Carer’s Week Research Report 
  (2020) estimated that around 1 in 4 people in the UK could be providing some 
  level of unpaid care, with around 74,000 estimated unpaid carers in the Leeds 
  City area (2011 Census/Carers Leeds, 2020).
  <br>
  <br>
  The needs of unpaid carers are known to be complex and varied, with surveys 
  finding that around 83% of respondents reported a negative impact of caring on
  their physical health, and 87% responding negative impacts on their mental 
  health (In Sickness and in Health, 2012). With so many people providing care 
  across Leeds and such high levels of reported needs nationally, it is 
  important to investigate the full health and social-care needs across Leeds, 
  and identify potential gaps and inequalities in service use.
  <br>
  <br>
  The Leeds NDL team has conducted and commissioned research to establish gaps 
  in the knowledge of both the Leeds population and Leeds CCG and Leeds City 
  Council teams regarding the needs of unpaid carers, and the utilisation of 
  services by unpaid carers. Priorities for unpaid carers were determined 
  initially via two insight reports, one conducted within team and one 
  commissioned separately. A Task and Finish group comprised of professionals 
  (council and third sector) who work with and on behalf of unpaid carers, and 
  unpaid carers from different economic and ethnic backgrounds honed the key 
  themes of the insight report, and pulled out questions on the most important 
  topics both to carers themselves and to the council for service design.</p>"
)

section1_text <- HTML(
  "<span style='font-size:20px'>
    So what demographic variation do we have in our GP carers data?
  </span>
  <br>
  <br> 
  <p>As mentioned above, in the 2011 census there were around 74,000 people 
  stating that they provided some level of unpaid care for another. If we look
  at data from the past five years, we find around 19,000 patients who have 
  informed their GP of their caring status: <font color='#E8B529'>around 25%
  of the expected number</font>. If we limit our time-window further, and only
  look for carer registrations within one year then we find between 6000-10,000
  patients looking back for each year from 2016-2021: <font color='#EE3E12'>
  an average of around 10% of the expected count</font>.
  <br>
  <br>
  Based on interactions with GP practices, common population health findings, 
  and anecdotal feedback from the Task and Finish group, we would not expect 
  this underrepresentation to be found equally across all demographic groups
  across the city, and so we compared our GP carer data with ONS mid-year 
  population estimates to highlight the specific groups which feature fewer 
  registered carers.
  <br>
  <br>
  In the figures below, please click on the legend items below the y-axis to 
  filter out the option. To highlight one legend option, please double-click the
  item.
  </p>")

text0 <- HTML("")

text1 <- HTML("<H2> Overall Population Proportion </H2>
              <br> <p> As mentioned above, the population proportion of carers
              falls well short of the expected figure from the census results.
              Prior to 2020 we can see a slight increase in the proportion of 
              registered patients providing care, rising from around
              <font color='#EE3E12'>1.08% in 2016</font> to 
              <font color='#00CCCC'>1.70% in 2019</font>.
              <br>
              <br>
              We see another large increase in <font color='#2B61DF'>2020
              (to 2.2%)</font>, before dropping to around
              <font color='#FF00FF'>1.45% in 2021</font>.
              <br> 
              <br></p>")

text2 <- HTML("<H2> Population Proportion by Sex </H2>
              <br> <p> In all years we see significantly more female carers than
              male carers, with roughly double the proportion of the female 
              population registering as carers than the male population.</p>")

text3 <- HTML("<H2> Population Proportion by Age Band </H2>
              <br> <p> From <font color='#EE3E12'>2016</font> to 
              <font color='#2B61DF'>2020</font> there is a roughly linear 
              increase in the proportion of carers with ten year age-band. 
              However, the population proportion difference is not static, with
              increasingly greater numbers of older carers registering than 
              younger carers. In <font color='#EE3E12'>2016, there were 
              approximately 9 times more elderly (80-89) carers than younger 
              (18-29) carers</font>, but <font color='#00CCCC'>by 2019 this difference had increased 
              to around 14 times</font>. In <font color='#2B61DF'>2020 this difference rose to nearly 19 
              times more elderly than younger carers</font>.
              <br>
              <br>
              However, this difference dropped off in 2021, with fewer elderly
              carers registering, but higher numbers of younger - middle-aged 
              carers.</p>")

text4 <- HTML("<H2> 2019 </H2>
              <br> <p>Workers with <font color='#3487BD'>associate's degrees</font> have a median income of $41,496.
              <br> On average, those occupations have a <b>50% chance</b> of job automation.
              <br><br> There are 1,869,840 workers with an <font color='#3487BD'>associate's degree</font>.<p>")

text5 <- HTML("<H2> Vaccine Drives </H2>
              <br> <p>Workers with <font color='#C71C7E'>bachelor's degrees</font> have a median income of $59,124.
              <br> On average, those occupations have a <b>20% chance</b> of job automation.
              <br><br> There are 18,399,270 workers with a <font color='#C71C7E'>bachelor's degree</font>.<p>")

text6 <- HTML("")

concludingtext <- HTML("")

technicalnotes <- HTML("")


### ALL PLOT OBJECTS

## Intro plot
# Intro static ggplot
introggPlot <- data %>%
  filter(age_band == 40) %>%
  ggplot(aes(text = round(snr, 2), text2 = round(snr_lcl, 2))) +
  geom_rect(
    aes(xmin = ymd('2016-09-01'), xmax = ymd('2016-11-01'), ymin = n_min, ymax = n_max),
    alpha = 0.02
  ) +
  geom_rect(
    aes(xmin = ymd('2017-09-01'), xmax = ymd('2017-11-01'), ymin = n_min, ymax = n_max),
    alpha = 0.02
  ) +
  geom_rect(
    aes(xmin = ymd('2018-09-01'), xmax = ymd('2018-11-01'), ymin = n_min, ymax = n_max),
    alpha = 0.02
  ) +
  geom_rect(
    aes(xmin = ymd('2019-09-01'), xmax = ymd('2019-11-01'), ymin = n_min, ymax = n_max),
    alpha = 0.02
  ) +
  geom_step(aes(gp_date, n, colour = sex), size = 1, position = position_nudge(-30)) +
  theme(legend.position = 'none') +
  geom_segment(
    aes(x = ymd('2018-03-01'), y = 50, xend = ymd('2016-11-01'), yend = 30),
    arrow = arrow(length = unit(0.5, "cm"))
  ) +
  geom_segment(
    aes(x = ymd('2018-03-01'), y = 50, xend = ymd('2017-11-01'), yend = 40),
    arrow = arrow(length = unit(0.5, "cm"))
  ) +
  geom_segment(
    aes(x = ymd('2018-03-01'), y = 50, xend = ymd('2018-11-01'), yend = 40),
    arrow = arrow(length = unit(0.5, "cm"))
  ) +
  geom_segment(
    aes(x = ymd('2018-03-01'), y = 50, xend = ymd('2019-11-01'), yend = 40),
    arrow = arrow(length = unit(0.5, "cm"))
  ) +
  annotate(
    'text',
    label = 'Signal (usual vaccine drive)',
    x = ymd('2018-03-01'),
    y = 53
  ) +
  labs(x = '', y = '')

# Convert into ggplotly
introPlot <- ggplotly(introggPlot, tooltip = 'text') %>%
  layout(
    title = element_blank(),
    legend = list(x = 0.65, y = 0.925),
    font = list(family = 'Lato'),
    margin = list(t=50),
    hoverlabel = list(bgcolor = 'whitesmoke', color = 'DarkGray')) %>% 
  config(displaylogo = F, showSendToCloud = F, displayModeBar = F)

## Carer deprivation score density
## Carer deprivation score density
deprivation_density <- carer_info %>%
  drop_na() %>% 
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

deprivationTotalPlot <- ggplotly(deprivation_density) %>%
  layout(
    title = element_blank(),
    font = list(family = 'Lato'),
    margin = list(t=50),
    hoverlabel = list(bgcolor = 'whitesmoke', color = 'DarkGray')) %>% 
  config(displaylogo = F, showSendToCloud = F, displayModeBar = F)

deprivationTotalPlot <- deprivationTotalPlot %>%
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
  id = seq_along(deprivationTotalPlot$x$data), 
  legend_entries = unlist(lapply(deprivationTotalPlot$x$data, `[[`, "name"))
)
df$legend_group <- gsub("^\\((.*?),\\d+\\)", "\\1", df$legend_entries)
df$is_first <- !duplicated(df$legend_group)

for (i in df$id) {
  is_first <- df$is_first[[i]]
  deprivationTotalPlot$x$data[[i]]$name <- df$legend_group[[i]]
  deprivationTotalPlot$x$data[[i]]$legendgroup <- deprivationTotalPlot$x$data[[i]]$name
  if (!is_first) deprivationTotalPlot$x$data[[i]]$showlegend <- FALSE
}

deprivation_male_density <- carer_info %>%
  drop_na() %>% 
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

deprivationPlot <- ggplotly(deprivation_male_density) %>%
  layout(
    title = element_blank(),
    font = list(family = 'Lato'),
    margin = list(t=50),
    hoverlabel = list(bgcolor = 'whitesmoke', color = 'DarkGray')) %>% 
  config(displaylogo = F, showSendToCloud = F, displayModeBar = F)

deprivationPlot <- deprivationPlot %>%
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

deprivationPlotF <- ggplotly(deprivation_female_density) %>%
  layout(
    title = element_blank(),
    font = list(family = 'Lato'),
    margin = list(t=50),
    hoverlabel = list(bgcolor = 'whitesmoke', color = 'DarkGray')) %>% 
  config(displaylogo = F, showSendToCloud = F, displayModeBar = F)

deprivationPlotF <- deprivationPlotF %>%
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

deprivationPlot <- subplot(deprivationPlot, deprivationPlotF)

# Making legends nicer
df <- data.frame(
  id = seq_along(deprivationPlot$x$data), 
  legend_entries = unlist(lapply(deprivationPlot$x$data, `[[`, "name"))
)
df$legend_group <- gsub("^\\((.*?),\\d+\\)", "\\1", df$legend_entries)
df$is_first <- !duplicated(df$legend_group)

for (i in df$id) {
  is_first <- df$is_first[[i]]
  deprivationPlot$x$data[[i]]$name <- df$legend_group[[i]]
  deprivationPlot$x$data[[i]]$legendgroup <- deprivationPlot$x$data[[i]]$name
  if (!is_first) deprivationPlot$x$data[[i]]$showlegend <- FALSE
}

# Age Band
deprivation_age_band_density <- carer_info %>%
  drop_na() %>% 
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

deprivationAgeBandPlot <- ggplotly(deprivation_age_band_density) %>%
  layout(
    title = element_blank(),
    font = list(family = 'Lato'),
    margin = list(t=50),
    hoverlabel = list(bgcolor = 'whitesmoke', color = 'DarkGray')) %>% 
  config(displaylogo = F, showSendToCloud = F, displayModeBar = F)

# Making legends nicer
df <- data.frame(
  id = seq_along(deprivationAgeBandPlot$x$data), 
  legend_entries = unlist(lapply(deprivationAgeBandPlot$x$data, `[[`, "name"))
)
df$legend_group <- gsub("^\\((.*?),\\d+\\)", "\\1", df$legend_entries)
df$is_first <- !duplicated(df$legend_group)

for (i in df$id) {
  is_first <- df$is_first[[i]]
  deprivationAgeBandPlot$x$data[[i]]$name <- df$legend_group[[i]]
  deprivationAgeBandPlot$x$data[[i]]$legendgroup <- deprivationAgeBandPlot$x$data[[i]]$name
  if (!is_first) deprivationAgeBandPlot$x$data[[i]]$showlegend <- FALSE
}

# NEL Distribution Plots
load(here('data/nelPlots.RData'))
nel_reference <- filter(nel_plots[[1]]$data, group)
load(here('data/compPlots.RData'))
