
# delete all objects
rm(list = ls())

library(dplyr)
library(here)
library(purrr)
library(tidyr)
library(tibble)
source(here::here("scripts", "fun_event_study_mine.R")) #adjust
source(here::here("scripts", "prepare_event_data_mine.R")) #adjust 

##Set Event study parameters 

estimation_window = 230 # days
estimation_window_end = -21  # days before the event
event_window_begin = -1
event_window_end = 1

#Read the data sets:
Acq_dates <- readRDS(here::here("data", "Acq_dates.rds"))
Acquiror_returns <- readRDS(here::here("data", "Acquiror_returns.rds"))
market_data <- readRDS(here::here("data", "market_data.rds"))


##########fix different market codes
us_exchanges <- c("New York Stock Exchange", "NYSE Amex", "NYSE Alter", "NYSE MKT", "Nasdaq", "Sm Cap Mkt", "Boston")
uk_exchange <- c("London", "London AIM", "Aquis Stock Exchange", "OFEX")
bd_exchange <- c("Frankfurt", "Berlin", "Düsseldorf", "Hamburg", "Xetra")

process_exchange_data <- function(data, exchange_column) {
  data <- data %>%
    mutate(
      !!exchange_column := case_when(
        !!sym(exchange_column) %in% us_exchanges ~ "US",
        !!sym(exchange_column) %in% uk_exchange  ~ "UK",
        !!sym(exchange_column) %in% bd_exchange  ~ "BD",
        TRUE ~ !!sym(exchange_column)
      )
    ) %>%
    filter(!!sym(exchange_column) %in% c("US", "UK", "BD")) %>%
    rename(country_code = !!sym(exchange_column))
  
  return(data)
}

Acq_dates <- process_exchange_data(Acq_dates, "country_code")
#Tar_dates <- process_exchange_data(Tar_dates, "country_code")

# create event ids to check for overlaps
Acq_dates <- Acq_dates %>%
  mutate(id = row_number())

  # create days id for form returns
Acquiror_returns <- Acquiror_returns %>%
  group_by(firm_id) %>%
  mutate( i = row_number())

  # compute the number of transaction days between events 
days_btwn_events <- Acq_dates %>%
  arrange(event_date)

days_btwn_events <- left_join(days_btwn_events, Acquiror_returns, by = c("firm_id", "event_date" = "date"))

days_btwn_events <- days_btwn_events %>%
  group_by(firm_id) %>%
  mutate(days_btwn_events = i - lag(i))

  #Exclude events of the same firm that occur within the interval of event_window_end and estimation_window_end
events_to_remove <- days_btwn_events %>%
  dplyr::filter(days_btwn_events <= event_window_end-estimation_window_end)

Acq_dates_no_overlap <- anti_join(Acq_dates, events_to_remove, by=c("event_date","firm_id", "id"))

#Prepare event data data frame 
all_event_data <- prepare_event_data(Acquiror_returns, 
                                     Acq_dates = Acq_dates_no_overlap,
                                     market_data,
                                     estimation_window,
                                     estimation_window_end,
                                     event_window_end)
#Extract components of the event study preparation
event_data <- all_event_data$event_data
data_for_models <- all_event_data$data_for_models
factor_data <- all_event_data$factor_data

#Perform the computation of the event study
event_study <- compute_event_study(event_data,
                                   estimation_window,
                                   estimation_window_end,
                                   event_window_begin,
                                   event_window_end,
                                   data_for_models,
                                   factor_data)
# Save the event study results
saveRDS(event_study, file = here::here("data", "results", "results_event_study.rds"))

results <- readRDS("/Users/saskiaschatzmayr/Uni/Master/Erasmus/Corporate_Finance/Group_project/data/results/results_event_study.rds")
