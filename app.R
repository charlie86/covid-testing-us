library(shiny)
library(tidyverse)
library(highcharter)

df <- readRDS('covid-testing-data.rds')

nationwide_plot_df <- df %>% 
    select(week, new_positive, new_tests) %>%
    group_by(week) %>%
    summarise(new_positive = sum(new_positive),
              new_tests = sum(new_tests),
              new_positive_rate = new_positive / new_tests) %>%
    ungroup() %>%
    filter(new_tests >= 1000, !is.na(new_positive))

ui <- fluidPage(
    fluidRow(
        column(12, align = 'center',
               h2('Share of new COVID-19 tests coming back positive by week', align = 'center'),
               selectInput('state', 'Select state(s)', c('Nationwide', state.name), selected = 'Nationwide', multiple = TRUE),
               highchartOutput('chart', height = '600px'),
               uiOutput('notes')
        )
    )
)

server <- function(input, output, session) {
    
    output$chart <- renderHighchart({
        
        state_plot_df <- df %>% 
            filter(state %in% input$state) %>% 
            group_by(state, week) %>%
            summarise(new_positive = sum(new_positive),
                      new_tests = sum(new_tests),
                      new_positive_rate = new_positive / new_tests) %>%
            ungroup() %>%
            filter(new_tests >= 1000, !is.na(new_positive))
        
        if ('Nationwide' %in% input$state) {
            plot_df <- nationwide_plot_df %>% 
                mutate(state = 'Nationwide') %>% 
                rbind(state_plot_df)    
        } else {
            plot_df <- state_plot_df
        }
        
        plot_df <- plot_df %>% 
            mutate(formatted_rate = scales::percent(new_positive_rate, accuracy = .1),
                   formatted_count = prettyNum(new_tests, big.mark = ','))
        
        hchart(plot_df, hcaes(x = week, y = new_positive_rate, group = state), type = 'line') %>% 
            hc_yAxis(title = list(text = ''), labels = list(style = list(fontSize = '16px'), formatter = JS('function() {return Math.round(this.value*100) + "%";}')), tickInterval = .05, allowDecimals = FALSE) %>% 
            hc_xAxis(title = list(text = ''), labels = list(style = list(fontSize = '16px'))) %>% 
            hc_tooltip(headerFormat = 'Week of {point.key}<br/>', 
                       style = list(fontSize = '14px'),
                       pointFormat = HTML('<span style="color:{point.color};">●</span> {series.name}: <b>{point.formatted_rate}</b> of {point.formatted_count} tests came back positive<br/>'), shared = T) %>% 
            hc_plotOptions(line = list(lineWidth = 4))
    })
    
    output$notes <- renderUI({
        HTML(str_glue('Data as of {format(max(df$date), "%B %d, %Y")}. Only weeks when there were at least 1,000 total tests run are included.<br>
                                         Source: <a href="https://covidtracking.com" target="_blank">The COVID Tracking Project</a>. Code: <a href="https://github.com/charlie86/covid-testing-us">GitHub</a>'))
    })
}

shinyApp(ui = ui, server = server)
