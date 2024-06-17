# DWP data
library(tidyverse)
library(lubridate)
library(geographr)
library(leaflet)
library(shiny)
library(sf)
library(xts)
library(config)
library(readxl)

options(stringsAsFactors = FALSE)
options(dplyr.summarise.inform = FALSE)

config <- get()

# Load custom functions
invisible(
  lapply(
    Sys.glob('user_defined_functions/*.R'),
    function(x) source(x)
  )
)

carer_allowance <- bind_rows(
  read_csv("data/carer_allowance_2003_2018.csv") %>% 
    select(-contains('- Anno')) %>%
    pivot_longer(-Quarter) %>%
    transmute(
      area = Quarter,
      date = my(name),
      total_allowances = value
    ),
  read_csv("data/carer_allowance_2018_present.csv") %>% 
    select(-contains('- Anno')) %>%
    pivot_longer(-Quarter) %>%
    transmute(
      area = Quarter,
      date = my(name),
      total_allowances = value
    )
)

carer_allowance2 <- carer_allowance %>%
  inner_join(lookup_lsoa11_msoa11, by = c('area' = 'lsoa11_name'))

msoa_allowance <- carer_allowance2 %>% 
  group_by(msoa = msoa11_code, date) %>%
  summarise(total_allowances = sum(as.numeric(total_allowances), na.rm = TRUE)) %>%
  ungroup()

date <- msoa_allowance %>%
  # filter(date >= ymd('2016-01-01')) %>%
  pull(date) %>%
  unique()

date_xts <- xts(1 : length(date), order.by = date)

allowance_range <- msoa_allowance %>%
  pull(total_allowances) %>%
  range()

colours <- colorNumeric('YlOrRd', allowance_range)

msoa_map <- msoa_allowance %>%
  left_join(boundaries_msoa11, by = c('msoa' = 'msoa11_code')) %>%
  st_as_sf() %>% 
  group_by(msoa) %>% 
  arrange(date) %>% 
  mutate(id = row_number()) %>%
  ungroup()

ui <- fluidPage(
  sliderInput(
    'time', 
    'date', 
    1, 
    length(date), 
    value = 1, 
    step = 1, 
    animate = T
  ), 
  leafletOutput('mymap')
)

server <- function(input, output, session) {
  tiles <- reactive({
    msoa_map %>%
      filter(id == input$time)
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = tiles(), 
        color = ~ colours(total_allowances),
        fillColor = ~ colours(total_allowances),
        fillOpacity = 0.9,
        weight = 1
      )
  })
}

shinyApp(ui, server)

#---- Population proportion ----

years <- as.character(2016:2021)

plots <- list()

imd_yearly <- population_rate_by_year(
  imd_decile,
  constant,
  as.character(2016:2021),
  gp_date,
  allc
)

la_imd_yearly <- population_rate_by_year(
  imd_decile,
  constant,
  as.character(2021),
  assessment_date,
  filter(carer_assessment2, !is.na(lsoa))
)


for (y in seq_along(years)) {
  year <- years[y]

  leeds_ca <- carer_allowance %>% 
    filter(date == ymd(paste(year, '05-01')))
  
  leeds_pop <- get_population(year) %>% 
    ungroup()
  
  leeds_pop <- leeds_pop %>%
    left_join(
      distinct(
        geographr::lookup_lsoa11_msoa11, 
        lsoa_name = lsoa11_name, 
        lsoa = lsoa11_code
      ),
      by = 'lsoa'
    ) %>%
    transmute(
      lsoa,
      lsoa_name = coalesce(lsoa_name.x, lsoa_name.y),
      year,
      count
    ) %>%
    filter(str_detect(lsoa_name, 'Leeds'))
  
  if (!exists('imd_2019')) imd_2019 <- calculate_deprivation_ntile()
  
  leeds_ca <- leeds_ca %>% 
    full_join(leeds_pop, by = c('area' = 'lsoa_name')) %>%
    left_join(imd_2019, by = 'lsoa') %>%
    mutate(
      total_allowances = coalesce(as.numeric(total_allowances), 0),
      count = coalesce(count, 0),
      proportion = 100 * total_allowances / count,
      lcl = proportion - 
        qnorm(1 - 0.05/2) * sqrt(proportion * (100 - proportion) / count),
      ucl = proportion +
        qnorm(1 - 0.05/2) * sqrt(proportion * (100 - proportion) / count)
    )
  
  plots[[y]] <- leeds_ca %>%
    group_by(imd = deprivation_decile) %>%
    summarise(
      proportion = 100 * sum(total_allowances) / sum(count),
      lcl = proportion - 
        qnorm(1 - 0.05/2) * sqrt(proportion * (100 - proportion) / sum(count)),
      ucl = proportion +
        qnorm(1 - 0.05/2) * sqrt(proportion * (100 - proportion) / sum(count))
    ) %>%
    filter(!is.na(imd)) %>%
    ggplot() +
    geom_errorbar(aes(imd, proportion, ymin = lcl, ymax = ucl)) +
    geom_errorbar(
      data = imd_yearly %>% filter(!is.na(constant), year == years[y]), 
      aes(imd_decile, 100 * proportion, ymin = 100 * lcl, ymax = 100 * ucl), 
      colour = 'blue'
    ) +
    geom_errorbar(
      data = la_imd_yearly %>% filter(!is.na(constant), year == years[y]), 
      aes(imd_decile, 100 * proportion, ymin = 100 * lcl, ymax = 100 * ucl), 
      colour = 'red'
    ) +
    ylim(0, 4) +
    ylab("Population [%]") +
    xlab("IMD Decile") +
    ggtitle(years[y])
}

do.call(gridExtra::grid.arrange, c(plots, ncol = 2))

