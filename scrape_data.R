library(rgdal)
library(rvest)
library(furrr)
library(scales)
library(polite)
library(tidyverse)
library(lubridate)
library(htmltools)
plan(multiprocess)

df <- future_map_dfr(c(state.name, 'Puerto Rico', 'District of Columbia'), function(state_name) {
    formatted_state_name <- tolower(gsub(' ', '-', state_name))
    session <- bow(str_glue('https://covidtracking.com/data/state/{formatted_state_name}'))
    scrape(session) %>%
        html_nodes('.table-module--table--1HfxU') %>% 
        .[[2]] %>% 
        html_table() %>% 
        mutate(date = str_sub(Date, 5, length(Date)),
               date = as.Date(date, '%b %d %Y')) %>% 
        select(-c(Date, `Screenshots (ET)`)) %>% 
        gather(metric, value, -date) %>% 
        mutate(value = parse_number(value)) %>% 
        spread(metric, value) %>% 
        set_names(gsub('\\.', '_', tolower(make.names(names(.))))) %>% 
        mutate(positive = total - negative,
               new_positive = positive - lag(positive),
               new_positive_rate = new_positive / new_tests,
               week = floor_date(date, unit = 'week'),
               state = state_name) 
}, .progress = TRUE)

state_map_plot_df <- df %>% 
    group_by(state) %>% 
    arrange(desc(date)) %>% 
    slice(1:7) %>% 
    summarise(new_positive = sum(new_positive, na.rm = TRUE),
              new_tests = sum(new_tests, na.rm = TRUE),
              new_positive_rate = new_positive / new_tests,
              non_null = sum(!is.na(positive))) %>%
    ungroup() %>% 
    filter(non_null > 0) %>% 
    arrange(-new_positive_rate) %>% 
    mutate(formatted_rate = scales::percent(new_positive_rate, accuracy = .1),
           formatted_count = prettyNum(new_tests, big.mark = ','),
           state_name = state,
           rank = row_number()) %>% 
    rowwise() %>% 
    mutate(label = HTML(paste0(
        '<b>', state, '</b>',
        '<br/>', 'In the past seven days ', formatted_count, ' tests were run.<br/>',
        '<b>', formatted_rate, '</b>', ' came back positive, the ', ordinal(rank), ' highest rate in the nation.' 
    ))) %>% 
    ungroup()

states <- readOGR("cb_2018_us_state_20m/cb_2018_us_state_20m.shp",
                  layer = "cb_2018_us_state_20m", GDAL1_integer64_policy = TRUE)

states <- states[states$NAME %in% state_map_plot_df$state, ]

states$new_positive_rate <- map(as.character(states$NAME), function(x) {
    this_row <- state_map_plot_df %>% filter(state_name == x)
    
    if (nrow(this_row) == 0) {
        rate <- NA
    } else {
        rate <- this_row$new_positive_rate
    }
    
    return(rate)
}) %>% unlist()

states$label <- map(as.character(states$NAME), function(x) {
    this_row <- state_map_plot_df %>% filter(state_name == x)
    
    if (nrow(this_row) == 0) {
        rate <- NA
    } else {
        rate <- this_row$label
    }
    
    return(rate)
}) %>% unlist()

saveRDS(df, file = 'covid-testing-data.rds')
saveRDS(states, file = 'covid-states-map.rds')
