# This library contains functions to prepare the event data.
# It creates a list with the following data frames
# - event_data
# - data_for_models
# - factor_data

prepare_event_data <- function(Acquiror_returns, 
                               Acq_dates, 
                               country_factor_data,
                               estimation_window,
                               estimation_window_end,
                               event_window_end){
  
  all_data <- left_join(Acquiror_returns, Acq_dates, 
                        by = c("date" = "event_date", "firm_id"))  #adjusted
  
  #identifies the event date for each firm/event 
  event_data <- all_data %>% 
    mutate(event_date_lead = if_else(!is.na(id), 0L, NA_integer_)) %>% 
    group_by(firm_id) %>% 
    mutate(nrow = row_number()) %>% 
    ungroup()
  
  #indentify the row number for each firm/event
  events_row_numbers <- event_data %>%
    dplyr::filter(event_date_lead == 0) %>%
    group_by(firm_id, id) %>%
    select(firm_id, id, nrow) %>%
    rename(event_row=nrow)
  
  #####checking for duplicates and deduplicating 
  #### maybe do this earlier? 
  Acquiror_returns %>%
    group_by(date, firm_id) %>%
    filter(n() > 1)
  
  Acq_dates %>%
    group_by(event_date, firm_id) %>%
    filter(n() > 1)
  Acquiror_returns <- Acquiror_returns %>% distinct(date, firm_id, .keep_all = TRUE)
  Acq_dates <- Acq_dates %>% distinct(event_date, firm_id, .keep_all = TRUE)
  
  
  event_data <- left_join(event_data, events_row_numbers,
                          by = c("firm_id")) %>%
    select(-c(id.x)) %>%
    rename(id = id.y)
  
  #calculate event date lead (days to event date)
  event_data <- event_data %>%
    group_by(firm_id, id) %>%
    mutate(event_date_lead = nrow - event_row) %>% 
    ungroup() %>%
    select(-c(nrow, event_row)) %>%
    dplyr::filter(!is.na(event_date_lead))
  
  # Filter dates within the estimation and event windows
  event_data <- event_data %>%
    dplyr::filter(event_date_lead %in% (estimation_window_end-estimation_window+1):event_window_end)
  
  # fill country_code to join with country_factor_data
  event_data <- event_data %>% #idk what happening here
    group_by(firm_id, id) %>% #idk what happening here
    tidyr::fill(c(country_code), .direction = "downup") %>%#idk what happening here
    ungroup()#idk what happening here
  
  # join with country returns  
  data_for_models <- event_data %>% 
    select(firm_id, country_code, id, date, ret) %>% 
    right_join(country_factor_data, by=c("date", "country_code")) %>% #idk if country code is necessary
    group_by(firm_id, id) %>% 
    nest()
  
  factor_data <- country_factor_data %>% 
    group_by(country_code, date) %>% #idk if country code is necessary 
    nest()
  
  return(list(event_data = event_data,
              data_for_models = data_for_models,
              factor_data = factor_data))
  
}