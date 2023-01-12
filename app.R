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
  tags$head(includeCSS("www/style.css")),
  
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
      sliderInput(
        "perct", "Percentage of Carers:",
        min = 0, max = 100, value = 50
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
        Please use the above slider to see how the age of our carer records 
        varies, and find the number of years we are required to look back to
        capture different percentages of carers.
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
      plotlyOutput('demographicsPlot', height = '400px'),
      HTML('</center>')
    ),
    scrolly_sections(
      HTML('<center>'),
      scrolly_section(id = 'total', render_text(1)),
      scrolly_section(id = 'sex', render_text(2)),
      scrolly_section(id = 'age_band', render_text(3)),
      scrolly_section(id = 'imd', render_text(1)),
      # scrolly_section(id = 'age_band_imd', render_text(1)),
      scrolly_section(id = 'language', render_text(1)),
      scrolly_section(id = 'assumed', render_text(1)),
      scrolly_section(id = "buffer", br()),
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
      textOutput("section"),
      br(),
      HTML('<center>'),
      plotlyOutput("timeSeriesPlot", height = '600px'),
      HTML('</center>')
    ),
    scrolly_sections(
      HTML('<center>'),
      scrolly_section(id = 0, render_text(2)),
      scrolly_section(id = 1, render_text(1)),
      scrolly_section(id = 2, render_text(1)),
      scrolly_section(id = 3, render_text(1)),
      scrolly_section(id = 4, render_text(1)),
      scrolly_section(id = 5, render_text(1)),
      # add a scrolly_section with nothing in it;
      # this buffer prevents the plot from disappearing while reading last section
      scrolly_section(id = "buffer", render_text(4)),
      scrolly_section(id = "d_total", render_text(4)),
      scrolly_section(id = "d_sex", render_text(4)),
      scrolly_section(id = "d_age_band", render_text(4)),
      scrolly_section(id = "buffer", br()),
      HTML('</center>'),
      width = '20%'
    )
  ),

  br(),
  
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
      scrolly_section(id = 0, render_text(2)),
      scrolly_section(id = 5, render_text(1)),
      scrolly_section(id = 10, render_text(1)),
      scrolly_section(id = 15, render_text(1)),
      scrolly_section(id = 20, render_text(1)),
      scrolly_section(id = 25, render_text(1)),
      scrolly_section(id = 30, render_text(1)),
      scrolly_section(id = 'original', render_text(1)),
      scrolly_section(id = 'optimal', render_text(1)),
      scrolly_section(id = "buffer", br()),
      HTML('</center>'),
      width = '20%'
    )
  )
)

# server
server <- function(input, output, session) {
  output$timeSeriesPlot <- renderPlotly({
    add <- input$timeSeries
    
    if(is.null(add)) add <- 0
    
    if (add < 4) {
      plot <- data %>%
        ggplot() +
          geom_step(aes(gp_date, n, colour = sex, alpha = add >= reveal), size = 1) +
          facet_wrap(~ age_band, scales = 'free_y', nrow = 2) +
          theme(legend.position = 'none') +
          labs(x = '', y = '') +
          ylim(0, NA)
    } else {
      plot <- data %>%
        ggplot() +
        geom_step(aes(gp_date, n, colour = sex), size = 1) +
        facet_wrap(~ age_band, scales = 'free_y', nrow = 2) +
        theme(legend.position = 'none') +
        labs(x = '', y = '') +
        ylim(0, NA)
    }
    
    if (add > 4) {
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
            grouping = as.numeric(grouping)
          )
      }
     
      demographPlot <- demographPlot %>% 
        ggplot(aes(grouping, 100 * proportion, colour = year)) + 
          geom_errorbar(
            aes(ymin = 100 * lcl, ymax = 100 * ucl)
          ) + 
        ylim(0, NA) +
        xlab(str_replace(group_id, 'total', 'population')) +
        ylab('Population [%]')
    } else if (group_id == 'age_band_imd') {
      demographPlot <- carer_registrations %>%
        filter(group == group_id) %>%
        ggplot(aes(imd_decile, 100 * proportion, colour = year)) + 
          geom_errorbar(
            aes(ymin = 100 * lcl, ymax = 100 * ucl)
          ) + 
          ylim(0, NA) +
          facet_wrap(~ age_band) +
          ylab('Population [%]')
    } else {
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
        ggplot() +
          geom_bar(aes(date, percentage, fill = language), stat = 'identity') +
          facet_wrap(~ carer) +
          ylab('Registered Population [%]') +
          xlab('')
    }
    
    ggplotly(demographPlot) %>%
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
    
    # demographPlot
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
    if (is.null(perct)) perct <- 50
    
    filtered_period <- carer_period %>% 
      filter(p <= perct)
    
    carer_period %>%
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
          '% of carers registrations'
        ),
        hjust = ifelse(max(filtered_period$year) < 8, -0.1, pmin(max(filtered_period$year) / 12, 1)),
        vjust = ifelse(max(filtered_period$year) < 8, 0, -1 + (perct / 200) * (perct > 95)),
        size = 6
      ) +
      theme(
        axis.text = element_text(size = rel(1.5)), 
        axis.title = element_text(size = rel(1.5))
      )
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
