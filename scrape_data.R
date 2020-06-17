library(rvest)
library(furrr)
library(polite)
library(tidyverse)
library(lubridate)
plan(multiprocess)


df <- future_map_dfr(state.name, function(state_name) {
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

saveRDS(df, file = 'covid-testing-data.rds')
