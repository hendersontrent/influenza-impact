#' Function for cleaning time series data
#' 
#' @param filepath quoted filepath to dataset
#' @author Trent Henderson

cleaner <- function(filepath){
  
  d <- read_excel(filepath) %>%
    gather(key = month, value = value, 2:13)
  
  d1 <- d %>%
    mutate(the_date = paste0("01-",month,"-",year)) %>%
    mutate(the_date_format = as.Date(the_date, "%d-%b-%Y")) %>%
    mutate(indicator = case_when(
      year == 2020 & month == "Aug" ~ "Remove",
      year == 2020 & month == "Sep" ~ "Remove",
      year == 2020 & month == "Oct" ~ "Remove",
      year == 2020 & month == "Nov" ~ "Remove",
      year == 2020 & month == "Dec" ~ "Remove",
      TRUE                          ~ "Keep")) %>% # No data for these months which distorts ts modelling
    filter(indicator != "Remove") %>%
    dplyr::select(-c(indicator)) %>%
    mutate(value = log(value)) %>%
    mutate(value = case_when(
      value == -Inf ~ log(min(value)),
      TRUE          ~ value)) %>%
    dplyr::select(c(the_date_format, value))
  
  return(d1)
}
