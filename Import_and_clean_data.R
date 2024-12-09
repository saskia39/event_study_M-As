
# delete all objects
rm(list = ls())

library(here)
library(tidyr)
library(dplyr)
library(readxl)
library(openxlsx)


#################################################
###### Import and clean data (Acq and Tar) ######
#################################################

process_data <- function(sheet_number, output_prefix) {
  # Read data
  data <- read_xlsx(file.path(here::here("data", "raw"), "DATA.xlsx"), sheet = sheet_number)
  
  # Clean and process data
  data$Code <- gsub("[[:punct:]]", "-", data$Code)
  data$Code <- gsub("-RI--U-", ".RI", data$Code)
  
  data <- separate(data, Code, c("firm_id", "variable"))
  
  data <- data %>%
    filter(!is.na(firm_id)) %>%
    select(-Name)
  
  # Reshape data to long format
  cols = c(2:ncol(data))
  data[, cols] <- lapply(data[, cols], as.numeric) #NA introduced
  
  data <- data %>%
    pivot_longer(-firm_id, names_to = "date", values_to = "ri", values_drop_na = TRUE)
  
  # Convert date to date format
  data <- data %>%
    mutate(date = as.numeric(date)) %>%
    mutate(date = as.Date(date, origin = "1899-12-30"))
  
  # Compute daily returns
  data <- data %>%
    group_by(firm_id) %>%
    mutate(ret = ri / lag(ri) - 1) %>%
    select(-ri)
  
  # Save results
  write.xlsx(data, file = here::here("data", paste0(output_prefix, "_returns.xlsx")))
  saveRDS(data, file = here::here("data", paste0(output_prefix, "_returns.rds")))
}

# Process acquiror data
process_data(sheet_number = 2, output_prefix = "Acquiror")

# Process target data
process_data(sheet_number = 3, output_prefix = "Target")


###################################
########## market data ############
###################################
mkt_data <- read.xlsx(file.path(here::here("data", "raw"), "DATA.xlsx"), sheet = 4)

mkt_data <- mkt_data %>%
  select(-Name)


mkt_data$Code <- gsub("[[:punct:]]", "-", mkt_data$Code)
mkt_data$Code <- gsub("(RI)", ".RI", mkt_data$Code)
mkt_data$Code <- gsub("TOTMK", "", mkt_data$Code)
mkt_data <- separate(mkt_data, Code, c("country_code", "VARIABLE"))

mkt_data <- mkt_data %>%
  filter(!is.na(country_code)) %>%
  select(-VARIABLE)

##### Reshape #####
cols = c(2:ncol(mkt_data))

mkt_data[,cols] <- mkt_data[,cols] %>% mutate_if(is.character, as.numeric)

mkt_data <- mkt_data %>% 
  pivot_longer(-country_code, names_to = "date", values_to = "ri",
               values_drop_na = TRUE)


##### Convert date to date formate #####
mkt_data <- mkt_data %>%
  mutate(date = as.numeric(date)) %>%
  mutate(date = as.Date(date, origin = "1899-12-30"))


###### Compute daily returns #####
mkt_data <- mkt_data %>%
  group_by(country_code) %>% 
  mutate(mkt_ret = ri/lag(ri)-1) %>%
  select(-ri)

write.xlsx(mkt_data, file = here::here("data", "market_data.xlsx"))
saveRDS(mkt_data, file=here::here("data", "market_data.rds"))


##########################################
###### Import and clean Event dates ######
##########################################

process_event_dates <- function(sheet_number, firm_id_col, event_date_col, country_code_col, output_prefix) {
  # Import data
  event_dates <- read.xlsx(file.path(here::here("data", "raw"), "DATA.xlsx"), sheet = sheet_number, startRow = 3)
  
  # Select and rename relevant columns
  event_dates <- event_dates %>%
    select(!!sym(firm_id_col), !!sym(event_date_col), !!sym(country_code_col)) %>%
    rename(firm_id = !!sym(firm_id_col), event_date = !!sym(event_date_col), country_code = !!sym(country_code_col))
  
  # Convert event_date to proper date format
  event_dates <- event_dates %>%
    mutate(event_date = as.numeric(event_date)) %>%
    mutate(event_date = as.Date(event_date, origin = "1899-12-30")) %>%
    drop_na()
  
  # Save outputs
  write.xlsx(event_dates, file = here::here("data", paste0(output_prefix, "_dates.xlsx")))
  saveRDS(event_dates, file = here::here("data", paste0(output_prefix, "_dates.rds")))
}

# Process Acquiror event dates
process_event_dates(sheet_number = 1, 
                    firm_id_col = "Acquiror.Datastream", 
                    event_date_col = "Date.Announced", 
                    country_code_col = "Acquiror.Primary.Stock.Exchange",
                    output_prefix = "Acq")

# Process Target event dates
process_event_dates(sheet_number = 1, 
                    firm_id_col = "Target.Datastream", 
                    event_date_col = "Date.Announced", 
                    country_code_col = "Target.Primary.Stock.Exchange", 
                    output_prefix = "Tar")


