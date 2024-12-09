# This library contains functions to calculate:
# - AR - Abnormal return
# - AAR - Average abnormal return
# - CAAR - Cumulative average abnormal return
# - Test statistics for those measures
#
# The main function takes a dataframe that has to include the following
# fields:
# - firm_id
# - date
# - ret
# - event window (with days, for instance -1:1)
#
# It computes expected returns, using the market model 
# Can be extended to included other models (e.g. CAPM) 
# 

require(dplyr)
require(tidyr)
require(purrr)



compute_event_study <- function(event_data, 
                                estimation_window,
                                estimation_window_end,
                                event_window_begin,
                                event_window_end,
                                data_for_models,
                                factor_data){
  
  # Fit expect returns models
  er <- event_data %>% 
    group_by(firm_id, id) %>% 
    dplyr::filter(event_date_lead == estimation_window_end) %>% 
    mutate(er_fit = pmap(list(firm_id, id, date), fit_er_models, data_for_models, estimation_window)) %>% 
    select(firm_id, id, ret, er_fit) 
  
  # Take only the days we were able to estimate the expected returns
  er <- er %>% 
    mutate(fit = if_else(is.na(er_fit[[1]]),0,1)) %>% 
    dplyr::filter(fit == 1, !is.na(ret)) %>%
    select(firm_id, id, er_fit) %>% 
    ungroup()
  
  # Get the event window to estimate abnormal returns
  events <- event_data %>% 
    dplyr::filter(event_date_lead %in% event_window_begin:event_window_end) %>%
    rename(event_window_day=event_date_lead)
  
  events <- right_join(events, er)
  
  # Keep only events if ret are all not na
  na_return_events <- events %>%
    filter(is.na(ret)) %>%
    select(firm_id, id)
  
  events <- anti_join(events, na_return_events)
  
  # Merge wth factor_data
  events <- events %>% 
    select(firm_id, country_code, id, date, ret, event_window_day, er_fit) %>% 
    left_join(factor_data) %>% 
    rename(factor_data = data)
  
  # Compute expected and abnormal returns 
  events <- events %>% 
    mutate(er = map2(er_fit, factor_data, compute_er)) %>% 
    mutate(ar = map2(er, ret, compute_ar)) %>%
    unnest(c(ar))
  
  # In the following it is assumed that a single (market) model is used (otherwise the code needs significant changes)
  # Prepare data for aggregation
  data_before_aggregating <- events %>% 
    rename(ar = ar_mkt) %>%
    select(-er_fit, -factor_data, -er) %>%
    group_by(firm_id, id) %>%
    mutate(car = cumsum(ar)) %>%
    ungroup()
 
  # Compute test statistics
  ev <- compute_tests(data_before_aggregating)
  
  
  # Get the companies considered in the event study
  number_events_per_firm <- er %>% 
    group_by(firm_id) %>% 
    tally()
  
  # Get number of companies per event
  number_firms_per_event <- er %>% 
    group_by(id) %>% 
    tally()
  
  return(list(data_before_aggregating = data_before_aggregating,
              ev = ev, 
              number_firms_per_event = number_firms_per_event,
              number_events_per_firm = number_events_per_firm))
}


# Fit the market model of expected returns
# other models can be added (e.g. CAPM) provided that data_for_models includes required factors
fit_er_models <- function(firmid, event_id, last_date, data_for_models, estimation_window){
  
  # select data from the estimation window
  firm_data <- data_for_models %>% 
    dplyr::filter(firm_id == firmid & id == event_id) %>% 
    unnest(cols = c(data)) %>% 
    dplyr::filter(date <= last_date) %>% 
    dplyr::filter(!is.na(ret))
  
  # fit the model only if the number of obs. equals the number of days of the estimation window
  if (NROW(firm_data) >= estimation_window){
    firm_data <- firm_data %>% 
      mutate(i = row_number()) %>% 
      top_n(estimation_window, i)
    
    mkt_model <- lm(ret ~ 1 + mkt_ret, firm_data)
  } else {
    mkt_model <- NA
  }
  
  er <- tibble_row(mkt_model = mkt_model)
  return(er)
}



# Compute expected return
compute_er <- function(er_fit, factor_data){
  
  er_mkt <- predict(er_fit$mkt_model[[1]], factor_data)
  # other models can be added 
  
  er <- tibble_row(er_mkt = er_mkt)
  return(er)  
}


# Compute abnormal return
compute_ar <- function(er, ret){
  
  ar_mkt <- ret - er$er_mkt
  # other models can be added 
  
  ar <- tibble_row(ar_mkt = ar_mkt)
  return(ar)  
}



compute_tests <- function(data){
  
  # Compute the mean, standard deviation, and number of obs.
  data <- data %>% 
    group_by(event_window_day) %>% 
    summarise_at(vars(ar, car), 
                 .funs = list(mean = mean, sd = sd, length = length)) %>% 
    set_names(gsub("(.*)_mean$", "a\\1", names(.))) %>%       
    set_names(gsub("(.*)_sd$", "c_sd_\\1", names(.))) %>% 
    set_names(gsub("(.*)_length$", "n_\\1", names(.)))
  
  # Compute the t-test
  # More robust tests can be used, e.g.: 
  # - BMP t-stat - Boehmer, E. (1991). Event-study methodology under conditions of event-induced variance. Journal of Financial Economics, 30(2), 253-272. doi:10.1016/0304-405x(91)90032-f
  
  data <- data %>% 
    mutate(t_aar = (aar*sqrt(n_ar))/c_sd_ar,
           p_t_aar = 2*pt(abs(t_aar), 
                          n_ar - 1, 
                          lower.tail = FALSE),
           t_acar = (acar*sqrt(n_car))/c_sd_car,
           p_t_acar = 2*pt(abs(t_acar), 
                           n_car - 1, 
                           lower.tail = FALSE))  
  
  return(data) 
}







