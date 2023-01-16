library(here)
source(here("scripts/source_code_for_shiny.R"))

ui <- fluidPage(
  
  title = "Unpaid Carers in Leeds",
  
  # meta tags
  meta() %>%
    meta_social(
      title = "Unpaid Carers in Leeds",
      description = "Investigation into the registration of and services provided to unpaid carers in Leeds",
      # url = "https://connorrothschild.shinyapps.io/automation/",
      image = "https://www.wypartnership.co.uk/application/files/cache/thumbnails/00265cf7e44ecd477739630e5cd56a39.png",
      image_alt = "Unpaid Carers in Leeds"
    ),
  
  # suppress warning messages while data is loading on-screen
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  ),
  tags$head(
    includeCSS("www/style.css"),
    tags$meta(charset = 'UTF-8')
  ),
  
  # article title & name
  fluidRow(
    HTML(
      "<center>
        <h1>Unpaid Carers in Leeds</h1>
        <p style='font-size:26px'> by Ben Alcock </p>
      </center>"
    )
  ),
  
  br(),
  br(),
  
  fluidRow(
    column(1),
    column(
      10,
      br(),
      intro_text,
      br(),
      hr()
    ),
    column(1)
  ),
  
  sidebarLayout(
    position = 'left',
    sidebarPanel(
      br(),
      tabsetPanel(
        id = 'lookbackPanel',
        tabPanel(
          'Percentage of Carers',
          sliderInput(
            "perct", "Percentage of Carers:",
            min = 0, max = 100, value = 50
          )
        ),
        tabPanel(
          'Years to Look Back',
          sliderInput(
            "years", "Years to look back:",
            min = 0, max = 20, value = 3
          )
        )
      ),
      br(),
      HTML(
        "<p>The main source of data for this analysis is primary care systems 
        (<font color='#A00042'>EMIS</font> and 
        <font color='#3487BD'>SystmOne</font>), but generally while we can find 
        trustworthy dates on which patients told their GPs that they were 
        providing care, we have relatively few identifiers for patients who have
        stopped providing care.
        <br>
        Looking back, we can see that a large proportion of carers were 
        identified a significant period of time ago, with most records being <b>
        at least three years old</b>, and a small proportion being older than
        50 years old.
        <br>
        <br>
        Please use the above sliders to see how the age of our carer records 
        varies, and find the number of years we are required to look back to
        capture different percentages of carers. Click between tabs to choose 
        either the percentage of carers you wish to capture, or the number of 
        years you wish to look back to find carers. Note that the graphs will 
        have slight differences due to rounding errors.
        </p>"
      )
    ),
    mainPanel(
      h2('Proportion of Carers Registered within Period'),
      br(),
      plotOutput("carerPeriod")
    )
  ),
  
  br(),
  
  fluidRow(
    column(1),
    column(
      10,
      br(),
      section1_text,
      br(),
      hr()
    ),
    column(1)
  ),
  
  # scrollytelling plot
  scrolly_container(
    "demographics",
    scrolly_graph(
      br(),
      br(),
      textOutput('section2'),
      br(),
      HTML('<center>'),
      actionButton("by_year", "Plot by Year"),
      actionButton("by_group", "Plot by Group"),
      plotlyOutput('demographicsPlot', height = '400px'),
      HTML('</center>')
    ),
    scrolly_sections(
      HTML('<center>'),
      scrolly_section(id = 'total', render_text(1)),
      scrolly_section(id = 'sex', render_text(2)),
      scrolly_section(id = 'age_band', render_text(3)),
      scrolly_section(id = 'imd', render_text(4)),
      # scrolly_section(id = 'age_band_imd', render_text(1)),
      scrolly_section(id = 'language', render_text(5)),
      scrolly_section(id = 'assumed', render_text(6)),
      scrolly_section(id = 'd_buffer', br()),
      HTML('</center>')
    )
  ),

  br(),

  # scrollytelling plot
  scrolly_container(
    "timeSeries",
    scrolly_graph(
      br(),
      br(),
      textOutput("section5"),
      br(),
      HTML('<center>'),
      plotlyOutput("timeSeriesPlot", height = '600px'),
      HTML('</center>')
    ),
    scrolly_sections(
      HTML('<center>'),
      scrolly_section(id = 0, render_text(7)),
      scrolly_section(id = 1, render_text(8)),
      scrolly_section(id = 2, render_text(9)),
      scrolly_section(id = 3, render_text(10)),
      scrolly_section(id = 4, render_text(11)),
      scrolly_section(id = 5, render_text(12)),
      # add a scrolly_section with nothing in it;
      # this buffer prevents the plot from disappearing while reading last section
      scrolly_section(id = "buffer", render_text(13)),
      scrolly_section(id = "d_total", render_text(14)),
      scrolly_section(id = "d_sex", render_text(15)),
      scrolly_section(id = "d_age_band", render_text(16)),
      scrolly_section(id = "d_buffer", br()),
      HTML('</center>'),
      width = '20%'
    )
  ),
  
  # scrollytelling plot
  scrolly_container(
    "timeSeriesCovid",
    scrolly_graph(
      br(),
      br(),
      textOutput("section"),
      br(),
      HTML('<center>'),
      actionButton('total_button', 'Overall IMD Shift'),
      actionButton('sex_button', 'IMD Shift by Sex'),
      actionButton('age_band_button', 'IMD Shift by Age-Band'),
      plotlyOutput("timeSeriesPlot2", height = '600px'),
      HTML('</center>')
    ),
    scrolly_sections(
      HTML('<center>'),
      scrolly_section(id = 6, render_text(17)),
      scrolly_section(id = "d_covid", render_text(18)),
      scrolly_section(id = 'd_buffer', br()),
      HTML('</center>'),
      width = '20%'
    )
  ),

  br(),
  
  fluidRow(
    column(1),
    column(
      10,
      br(),
      nel_text,
      br(),
      hr()
    ),
    column(1)
  ),

    # scrollytelling plot
  scrolly_container(
    "nelPlots",
    scrolly_graph(
      br(),
      br(),
      textOutput("section3"),
      br(),
      HTML('<center>'),
      plotlyOutput("nelPlot", height = '600px'),
      HTML('</center>')
    ),
    scrolly_sections(
      HTML('<center>'),
      scrolly_section(id = 0, render_text(19)),
      scrolly_section(id = 5, render_text(20)),
      scrolly_section(id = 10, render_text(21)),
      scrolly_section(id = 15, render_text(22)),
      scrolly_section(id = 20, render_text(23)),
      scrolly_section(id = 25, render_text(24)),
      # scrolly_section(id = 30, render_text(25)),
      scrolly_section(id = 'original', render_text(25)),
      scrolly_section(id = 'optimal', render_text(26)),
      scrolly_section(id = 'd_buffer', br()),
      HTML('</center>'),
      width = '20%'
    )
  )
)

# server
server <- function(input, output, session) {
  deprivationPlotGroup <- reactiveValues(data = 'total_button')
  
  demographPlotX <- reactiveValues(data = 'by_year')
  
  observeEvent(input$by_year, {
    demographPlotX$data <- 'by_year'
  })
  
  observeEvent(input$by_group, {
    demographPlotX$data <- 'by_group'
  })
  
  observeEvent(input$total_button, {
    deprivationPlotGroup$data <- 'total_button'
  })
  
  observeEvent(input$sex_button, {
    deprivationPlotGroup$data <- 'sex'
  })
  
  observeEvent(input$age_band_button, {
    deprivationPlotGroup$data <- 'age_band'
  })
  
  output$timeSeriesPlot <- renderPlotly({
    add <- input$timeSeries
    
    if(is.null(add)) add <- 0
    
    if (add < 4) {
      plot <- data %>%
        ggplot() +
          geom_step(
            aes(gp_date, n, colour = sex, alpha = add >= reveal), 
            size = 1,
            direction = 'vh'
          ) +
          facet_wrap(~ age_band, scales = 'free_y', nrow = 2) +
          theme(legend.position = 'none') +
          labs(x = '', y = '') +
          ylim(0, NA)
    } else {
      plot <- data %>%
        ggplot() +
        geom_step(aes(gp_date, n, colour = sex), size = 1, direction = 'vh') +
        facet_wrap(~ age_band, scales = 'free_y', nrow = 2) +
        theme(legend.position = 'none') +
        labs(x = '', y = '') +
        ylim(0, NA)
    }
    
    if (add == 5) {
      plot <- plot +
        geom_rect(
          aes(xmin = ymd('2016-09-01'), xmax = ymd('2016-11-01'), ymin = n_min, ymax = n_max),
          alpha = 0.2
        ) +
        geom_rect(
          aes(xmin = ymd('2017-09-01'), xmax = ymd('2017-11-01'), ymin = n_min, ymax = n_max),
          alpha = 0.2
        ) +
        geom_rect(
          aes(xmin = ymd('2018-09-01'), xmax = ymd('2018-11-01'), ymin = n_min, ymax = n_max),
          alpha = 0.2
        ) +
        geom_rect(
          aes(xmin = ymd('2019-09-01'), xmax = ymd('2019-11-01'), ymin = n_min, ymax = n_max),
          alpha = 0.2
        )
    }
    
    if (add > 5) {
      plot <- covid_data %>%
        ggplot() +
        geom_step(aes(gp_date, n, colour = sex), size = 1, direction = 'vh') +
        facet_wrap(~ age_band, scales = 'free_y', nrow = 2) +
        theme(legend.position = 'none') +
        labs(x = '', y = '') +
        ylim(0, NA) +
        geom_rect(
          aes(xmin = ymd('2021-01-01'), xmax = ymd('2021-03-01'), ymin = n_min, ymax = n_max),
          data = filter(covid_data, age_band < 70),
          alpha = 0.2
        ) +
        geom_rect(
          aes(xmin = ymd('2020-03-01'), xmax = ymd('2020-05-01'), ymin = n_min, ymax = n_max),
          data = filter(covid_data, age_band > 50),
          alpha = 0.2
        )
    }
    
    if (!add %in% c('buffer', 'd_total', 'd_sex', 'd_age_band')) {
      ggplotly(plot) %>%
        layout(
          title = list(element_blank()),
          legend = list(x = 0.65, y = 0.925),
          font = list(family = 'Lato'),
          margin = list(t = 50),
          hoverlabel = list(bgcolor = 'whitesmoke', color = 'darkGray')
        ) %>%
        config(
          displaylogo = F,
          showSendToCloud = F,
          displayModeBar = F
        ) %>% 
        return()
    } else if (add == 'buffer') {
      return(introPlot)
    } else {
      if (add == 'd_total') return(deprivationTotalPlot)
      if (add == 'd_sex') return(deprivationPlot)
      if (add == 'd_age_band') return(deprivationAgeBandPlot)
      
      if (add == 'd_covid') {
        browser()
        if (deprivationPlotGroup$data == 'total') return(deprivationTotalPlotCovid)
        if (deprivationPlotGroup$data == 'total') return(deprivationPlotcovid)
        if (deprivationPlotGroup$data == 'total') return(deprivationAgeBandPlotCovid)
      }
    }
  })
  
  output$timeSeriesPlot2 <- renderPlotly({
    add <- input$timeSeriesCovid
    
    req(add)
    
    if (!is.na(as.numeric(add))) {
      plot <- covid_data %>%
        ggplot() +
        geom_step(aes(gp_date, n, colour = sex), size = 1, direction = 'vh') +
        facet_wrap(~ age_band, scales = 'free_y', nrow = 2) +
        theme(legend.position = 'none') +
        labs(x = '', y = '') +
        ylim(0, NA) +
        geom_rect(
          aes(xmin = ymd('2021-01-01'), xmax = ymd('2021-03-01'), ymin = n_min, ymax = n_max),
          data = filter(covid_data, age_band < 70),
          alpha = 0.2
        ) +
        geom_rect(
          aes(xmin = ymd('2020-03-01'), xmax = ymd('2020-05-01'), ymin = n_min, ymax = n_max),
          data = filter(covid_data, age_band > 50),
          alpha = 0.2
        )
    } else {
      if (deprivationPlotGroup$data == 'total_button') return(deprivationTotalPlotCovid)
      if (deprivationPlotGroup$data == 'sex') return(deprivationPlotCovid)
      if (deprivationPlotGroup$data == 'age_band') return(deprivationAgeBandPlotCovid)
    }
  })
  
  output$demographicsPlot <- renderPlotly({
    group_id <- input$demographics
    
    if (is.null(group_id)) group_id <- 'total'
    
    if (!group_id %in% c('age_band_imd', 'language', 'assumed')) {
      demographPlot <- carer_registrations %>%
        filter(group == group_id) 
      
      if (group_id == 'imd') {
        demographPlot <- demographPlot %>%
          mutate(
            grouping = as.integer(grouping)
          )
      }
      
      if (demographPlotX$data == 'by_year') {
        demographPlot <- demographPlot %>% 
          ggplot(
            aes(
              year, 
              100 * proportion, 
              colour = factor(grouping),
              text = paste0(
                'Year: ', year,
                '\n', str_to_title(str_replace_all(group_id, '_', ' ')), ': ', grouping,
                '\nPopulation Percentage: ', round(100 * proportion, 2), '%',
                '\nLCL [95%]: ', round(100 * lcl, 2), '%',
                '\nUCL [95%]: ', round(100 * ucl, 2), '%'
              )
            )
          ) + 
          geom_errorbar(
            aes(ymin = 100 * lcl, ymax = 100 * ucl)
          ) + 
          geom_line(size = 1) +
          ylim(0, NA) +
          scale_color_discrete(name = str_replace(group_id, 'total', 'population')) +
          xlab('Year') +
          ylab('Population [%]')
      } else {
        demographPlot <- demographPlot %>% 
          ggplot(
            aes(
              grouping, 
              100 * proportion, 
              colour = year,
              text = paste0(
                'Year: ', year,
                '\n', str_to_title(str_replace_all(group_id, '_', ' ')), ': ', grouping,
                '\nPopulation Percentage: ', round(100 * proportion, 2), '%',
                '\nLCL [95%]: ', round(100 * lcl, 2), '%',
                '\nUCL [95%]: ', round(100 * ucl, 2), '%'
              )
            )
          ) + 
            geom_errorbar(
              aes(ymin = 100 * lcl, ymax = 100 * ucl)
            ) + 
            geom_line(size = 1) +
            ylim(0, NA) +
            xlab(str_replace(group_id, 'total', 'population')) +
            ylab('Population [%]')
      }
    } else if (group_id == 'age_band_imd') {
      demographPlot <- carer_registrations %>%
        filter(group == group_id) %>%
        ggplot(
          aes(
            imd_decile, 
            100 * proportion, 
            colour = year,
            text = paste0(
              'Year: ', year,
              '\n', str_to_title(str_replace_all(group_id, '_', ' ')), ': ', grouping,
              '\nPopulation Percentage: ', round(100 * proportion, 2), '%',
              '\nLCL [95%]: ', round(100 * lcl, 2), '%',
              '\nUCL [95%]: ', round(100 * ucl, 2), '%'
            )
          )
        ) + 
          geom_errorbar(
            aes(ymin = 100 * lcl, ymax = 100 * ucl)
          ) + 
          ylim(0, NA) +
          facet_wrap(~ age_band) +
          ylab('Population [%]')
    } else {
      # hide('by_year')
      # hide('by_group')
      
      demographPlot <- carer_language %>%
        filter(
          !language %in% unlist(
            ifelse(
              group_id == 'language', 
              'assumed_english',
              list(c('english', 'unknown'))
            )
          )
        ) %>%
        mutate(
          language = str_to_title(str_replace_all(language, '_', ' ')),
          carer = if_else(carer, 'Carer', 'Non-Carer')
        ) %>%
        group_by(date, carer) %>%
        mutate(
          text = ifelse(
            group_id == 'language',
            paste0(
              'Month: ', date,
              '\nEnglish: ', round(percentage[language == 'English'], 2), '%',
              '\nNon-English: ', round(percentage[language == 'Non English'], 2), '%',
              '\nUnknown: ', round(percentage[language == 'Unknown'], 2), '%'
            ),
            paste0(
              'Month: ', date,
              '\nAssumed English: ', round(percentage[language == 'Assumed English'], 2), '%',
              '\nNon-English: ', round(percentage[language == 'Non English'], 2), '%'
            )
          )
        ) %>%
        ggplot(aes(text = text)) +
          geom_bar(aes(date, percentage, fill = language), stat = 'identity') +
          facet_wrap(~ carer) +
          ylab('Registered Population [%]') +
          xlab('')
    }
    
    ggplotly(demographPlot, tooltip = 'text') %>%
      layout(
        title = list(element_blank()),
        legend = list(orientation = 'h', xanchor = 'center', x = 0.5, y = -0.2),
        font = list(family = 'Lato'),
        margin = list(t = 50),
        hoverlabel = list(bgcolor = 'whitesmoke', color = 'darkGray')
      ) %>%
      config(
        displaylogo = F,
        showSendToCloud = F,
        displayModeBar = F
      )
  })
  
  output$nelPlot <- renderPlotly({
    offset <- input$nelPlots

    if (is.null(offset)) offset <- 0
    
    if (!is.na(as.numeric(offset))) {
      nelPlot <- nel_plots[[as.numeric(offset) + 1]] + 
        scale_fill_discrete(name = 'Carer') +
        xlab('Cambridge Emergency Admission Score') + 
        annotate(
          'text', 
          label = paste('Age carers by', offset, 'years'), 
          size = 4, 
          x = 600, 
          y = 0.005
        ) +
        geom_density(aes(nel), data = nel_reference, kernel = 'rectangular', size = 0.5)
    } else {
      nelPlot <- comp_plot[[2 - (offset == 'original')]] +
        scale_fill_discrete(name = 'Carer') +
        xlab('Cambridge Emergency Admission Score')
    }
    
    ggplotly(nelPlot) %>%
      layout(
        title = list(element_blank()),
        legend = list(x = 0.925, y = 0.925),
        font = list(family = 'Lato'),
        margin = list(t = 50),
        hoverlabel = list(bgcolor = 'whitesmoke', color = 'darkGray')
      ) %>%
      config(
        displaylogo = F,
        showSendToCloud = F,
        displayModeBar = F
      )
    
    # nelPlot
  })
  
  output$carerPeriod <- renderPlot({
    perct <- input$perct
    years <- input$years
    
    if (input$lookbackPanel == 'Percentage of Carers') {
      filtered_period <- carer_period %>% filter(p <= perct)
      
      period_plot <- carer_period %>%
        ggplot(aes(x = year, y = p)) + 
        geom_line(size = 1) + 
        xlim(0, 20) + 
        geom_ribbon(
          aes(year, ymax = p), 
          data = filtered_period, 
          ymin = 0, 
          fill = 'red', 
          alpha = 0.4
        ) + 
        geom_segment(
          aes(xend = -Inf, yend = p), 
          data = filter(filtered_period, year == max(year))
        ) + 
        geom_segment(
          aes(xend = year, yend = -Inf), 
          data = filter(filtered_period, year == max(year))
        ) +
        ylab('Carers Registered [%]') +
        xlab('Years before 2022') +
        annotate(
          'text',
          x = pmin(max(filtered_period$year), 20),
          y = perct,
          label = paste0(
            'We need to look back ', 
            round(max(filtered_period$year)), 
            ' years to capture ', 
            perct, 
            '% of carer registrations'
          ),
          hjust = ifelse(max(filtered_period$year) < 8, -0.1, pmin(max(filtered_period$year) / 12, 1)),
          vjust = ifelse(max(filtered_period$year) < 8, 0, -1 + (perct / 200) * (perct > 95)),
          size = 6
        ) +
        theme(
          axis.text = element_text(size = rel(1.5)), 
          axis.title = element_text(size = rel(1.5))
        )
    } else {
      filtered_period <- carer_period %>% filter(ceiling(year) <= years)
      
      period_plot <- carer_period %>%
        ggplot(aes(x = p, y = year)) + 
        geom_line(size = 1) + 
        geom_ribbon(
          aes(p, ymax = year), 
          data = filtered_period, 
          ymin = 0, 
          fill = 'red', 
          alpha = 0.4
        ) + 
        geom_segment(
          aes(xend = p, yend = -Inf), 
          data = filter(filtered_period, p == max(p))
        ) + 
        geom_segment(
          aes(xend = -Inf, yend = year), 
          data = filter(filtered_period, p == max(p))
        ) +
        ylim(0, 20) +
        xlab('Carers Registered [%]') +
        ylab('Years before 2022') +
        annotate(
          'text',
          x = pmin(pmax(mean(round(filtered_period$p)), 0), 40),
          y = pmin(pmax(years + 2, 5), 15),
          label = paste0( 
            round(max(filtered_period$p)), 
            '% of carers were registered within the past ', 
            years, 
            ' years'
          ),
          size = 6,
          hjust = -0.1,
        ) +
        theme(
          axis.text = element_text(size = rel(1.5)), 
          axis.title = element_text(size = rel(1.5))
        )
    }
    
    return(period_plot)
  })
  
  output$demographics <- renderScrollytell({
    scrollytell()
  })
  output$timeSeries <- renderScrollytell({
    scrollytell()
  })
  output$nelPlots <- renderScrollytell({
    scrollytell()
  })
  output$timeSeriesCovid <- renderScrollytell({
    scrollytell()
  })
  renderText(
    paste0(
      "Section: ", 
      coalesce(
        input$demographics,
        input$timeSeries,
        input$nelPlots
      )
    )
  )
  observe({
    cat("section:", coalesce(
      null_na(input$demographics),
      null_na(input$timeSeries),
      null_na(input$nelPlots)
    ), "\n")
  })
}
# Run the application
shinyApp(ui = ui, server = server)
