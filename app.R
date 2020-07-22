library(shiny)
library(scales)
library(leaflet)
library(tidyverse)
library(highcharter)
library(leaflet.extras)

df <- readRDS('covid-testing-data.rds')
states <- readRDS('covid-states-map.rds')

ui <- fluidPage(title = '',
    fluidRow(column(12, align = 'center', h2('Share of new COVID-19 tests coming back positive in the U.S.'))),
    br(),
    fluidRow(
        column(6, align = 'center',
               h4('Positive test rate over time', align = 'center'),
               div(style = 'display:inline-block;',
                   div(style = 'display:inline-block;', selectInput('state', 'Select state(s)', c('Nationwide', state.name), selected = 'Nationwide', multiple = TRUE)),
                   div(style = 'display:inline-block;', selectInput('frequency', 'Frequency', c('Daily', 'Weekly'))),
               ),
               highchartOutput('chart', height = '550px')
        ),
        column(6, 
               h4('Positive test rate by state - previous seven days', align = 'center'),
               h6('Click a state to add to time series chart', align = 'center'),
               leafletOutput('map', height = '600px')
               )
        ),
    fluidRow(column(12, align = 'center', uiOutput('notes')))
)

server <- function(input, output, session) {
    
    output$chart <- renderHighchart({
        
        if (input$frequency == 'Weekly') {
            state_plot_df <- df %>% 
                filter(state %in% input$state) %>% 
                group_by(state, week) %>%
                summarise(new_positive = sum(new_positive),
                          new_tests = sum(new_tests),
                          new_positive_rate = new_positive / new_tests) %>%
                ungroup() %>%
                filter(new_tests >= 1000, !is.na(new_positive))
            
            if ('Nationwide' %in% input$state) {
                
                nationwide_plot_df <- df %>% 
                    select(week, new_positive, new_tests) %>%
                    group_by(week) %>%
                    summarise(new_positive = sum(new_positive, na.rm = T),
                              new_tests = sum(new_tests, na.rm = T),
                              new_positive_rate = new_positive / new_tests) %>%
                    ungroup() %>%
                    filter(new_tests >= 1000, !is.na(new_positive))
                
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
                hc_yAxis(title = list(text = ''), labels = list(style = list(fontSize = '16px'), formatter = JS('function() {return Math.round(this.value*100) + "%";}')), tickInterval = .05, allowDecimals = FALSE, ceiling = 1) %>% 
                hc_xAxis(title = list(text = ''), labels = list(style = list(fontSize = '16px'))) %>% 
                hc_tooltip(headerFormat = 'Week of {point.key}<br/>', 
                           style = list(fontSize = '14px'),
                           pointFormat = HTML('<span style="color:{point.color};">●</span> {series.name}: <b>{point.formatted_rate}</b> of {point.formatted_count} tests came back positive<br/>'), shared = T) %>% 
                hc_plotOptions(line = list(lineWidth = 4))
            
        } else if (input$frequency == 'Daily') {
            state_plot_df <- df %>% 
                filter(state %in% input$state) %>% 
                group_by(state, date) %>%
                summarise(new_positive = sum(new_positive),
                          new_tests = sum(new_tests),
                          new_positive_rate = new_positive / new_tests) %>%
                ungroup() %>%
                filter(new_tests >= 1000, !is.na(new_positive))
            
            if ('Nationwide' %in% input$state) {
                
                nationwide_plot_df <- df %>% 
                    select(date, new_positive, new_tests) %>%
                    group_by(date) %>%
                    summarise(new_positive = sum(new_positive, na.rm = T),
                              new_tests = sum(new_tests, na.rm = T),
                              new_positive_rate = new_positive / new_tests) %>%
                    ungroup() %>%
                    filter(new_tests >= 1000, !is.na(new_positive))
                
                plot_df <- nationwide_plot_df %>% 
                    mutate(state = 'Nationwide') %>% 
                    rbind(state_plot_df)    
            } else {
                plot_df <- state_plot_df
            }
            
            plot_df <- plot_df %>% 
                mutate(formatted_rate = scales::percent(new_positive_rate, accuracy = .1),
                       formatted_count = prettyNum(new_tests, big.mark = ','))
            
            hchart(plot_df, hcaes(x = date, y = new_positive_rate, group = state), type = 'line') %>% 
                hc_yAxis(title = list(text = ''), labels = list(style = list(fontSize = '16px'), formatter = JS('function() {return Math.round(this.value*100) + "%";}')), tickInterval = .05, allowDecimals = FALSE, ceiling = 1) %>% 
                hc_xAxis(title = list(text = ''), labels = list(style = list(fontSize = '16px'))) %>% 
                hc_tooltip(headerFormat = '{point.key}<br/>', 
                           style = list(fontSize = '14px'),
                           pointFormat = HTML('<span style="color:{point.color};">●</span> {series.name}: <b>{point.formatted_rate}</b> of {point.formatted_count} tests came back positive<br/>'), shared = T) %>% 
                hc_plotOptions(line = list(lineWidth = 4))
        }
        
    })
    
    output$notes <- renderUI({
        HTML(str_glue('Data as of {format(max(df$date), "%B %d, %Y")}. Only weeks when there were at least 1,000 total tests run are included.<br>
                                         Source: <a href="https://covidtracking.com" target="_blank">The COVID Tracking Project</a>. Code: <a href="https://github.com/charlie86/covid-testing-us">GitHub</a>'))
    })
    
    output$map <- renderLeaflet({
        
        pal <- colorNumeric('Reds', states$new_positive_rate)
        
        leaflet(states) %>% 
            setView(-95, 39, zoom = 4) %>% 
            setMapWidgetStyle(list(background = "white")) %>% 
            addPolygons(labelOptions = labelOptions(textsize = "15px"),
                        highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE),
                        smoothFactor = 0.5, fillOpacity = 1,
                        color = 'grey', weight = 1,
                        fillColor = ~pal(new_positive_rate),
                        layerId = ~NAME,
                        label = ~map(label, HTML)
                        ) %>% 
            addLegend(pal = pal, values = states$new_positive_rate, opacity = 1,
                      labFormat = labelFormat(transform = function(x) x * 100, suffix = '%'),
                      title = HTML('Positive test rate'), position = 'bottomright')
        
    })
    
    observeEvent(input$map_shape_click, {
        updateSelectInput(session, 'state', selected = c(input$state, input$map_shape_click$id))
    })
}

shinyApp(ui = ui, server = server)
