#----------------------------------------
# This script sets out to load and prep
# the data for a multivariate causal
# impact analysis
#----------------------------------------

#----------------------------------------
# Author: Trent Henderson, 3 August 2020
#----------------------------------------

#-----------------INFLUENZA-----------------------------------------

# Filepaths

flupath <- "data/flu.xlsx"
hepbpath <- "data/hepb.xlsx"
hepcpath <- "data/hepc.xlsx"
chickenpoxpath <- "data/chickenpox.xlsx"

# Run it

flu <- cleaner(flupath) %>%
  rename(flu = value)

the_av <- flu %>%
  filter(the_date_format == as.Date("2002-04-01") | the_date_format == as.Date("2003-04-01")) %>%
  mutate(category = 1) %>%
  group_by(category) %>%
  summarise(av = mean(flu)) %>%
  ungroup()

the_val <- as.vector(the_av$av) 

flu <- flu %>%
  mutate(flu = case_when(
         the_date_format == as.Date("2001-04-01") ~ the_val,
         TRUE                                     ~ flu))

#-----------------HEPB----------------------------------------------

hepb <- cleaner(hepbpath) %>%
  rename(hepb = value)

#-----------------HEPC----------------------------------------------

hepc <- cleaner(hepcpath) %>%
  rename(hepc = value)

#-----------------CHICKENPOX----------------------------------------

chickenpox <- cleaner(chickenpoxpath) %>%
  rename(chickenpox = value)

# Merge all together

diseases <- flu %>%
  left_join(hepb, by = c("the_date_format" = "the_date_format")) %>%
  left_join(hepc, by = c("the_date_format" = "the_date_format"))

#-----------------EMPLOYMENT----------------------------------------

employed <- read_excel("data/employed.xlsx", sheet = 2) %>%
  rename(the_date_format = date) %>%
  mutate(the_date_format = as.Date(the_date_format))

#-----------------TRAVEL--------------------------------------------

travel <- read_excel("data/flights.xlsx", sheet = 2) %>%
  clean_names() %>%
  dplyr::select(c(1:3)) %>%
  drop_na() %>%
  mutate(monthcode = case_when(
         month == "Jan" ~ "01",
         month == "Feb" ~ "02",
         month == "Mar" ~ "03",
         month == "Apr" ~ "04",
         month == "May" ~ "05",
         month == "Jun" ~ "06",
         month == "Jul" ~ "07",
         month == "Aug" ~ "08",
         month == "Sep" ~ "09",
         month == "Oct" ~ "10",
         month == "Nov" ~ "11",
         month == "Dec" ~ "12")) %>%
  mutate(the_date_format = paste0(year,"-",monthcode,"-01")) %>%
  mutate(the_date_format = as.Date(the_date_format)) %>%
  dplyr::select(c(the_date_format, hours)) %>%
  rename(flight_hours = hours)

#-----------------GOOGLE TRENDS-------------------------------------

google <- read_excel("data/flu_google_trends.xlsx") %>%
  clean_names() %>%
  rename(searches = 2) %>%
  mutate(the_date_format = paste0(month,"-01")) %>%
  mutate(the_date_format = as.Date(the_date_format)) %>%
  dplyr::select(c(the_date_format, searches)) %>%
  mutate(searches = gsub("<","",searches)) %>%
  mutate(searches = as.numeric(searches))

#-----------------WEATHER-------------------------------------------

weather <- read_excel("data/aus_temp.xlsx") %>%
  gather(key = month, value = value, 2:13) %>%
  clean_names() %>%
  mutate(the_date = paste0("01-",month,"-",year)) %>%
  mutate(the_date_format = as.Date(the_date, "%d-%b-%Y")) %>%
  mutate(indicator = case_when(
    year == 2020 & month == "Nov" ~ "Remove",
    year == 2020 & month == "Dec" ~ "Remove",
    TRUE                          ~ "Keep")) %>% # No data for these months which distorts ts modelling
  filter(indicator != "Remove") %>%
  dplyr::select(-c(indicator)) %>%
  mutate(value = as.numeric(value)) %>%
  dplyr::select(c(the_date_format, value)) %>%
  rename(perth_temp = value) %>%
  drop_na()

#-----------------SALES---------------------------------------------

sales <- read_excel("data/sales_data.xlsx") %>%
  clean_names() %>%
  rename(the_date_format = date) %>%
  mutate(the_date_format = as.Date(the_date_format)) %>%
  rename(food_turnover = 2) %>%
  rename(household_turnover = 3) %>%
  rename(clothing_turnover = 4) %>%
  rename(dept_turnover = 5) %>%
  rename(retailing_turnover = 6) %>%
  rename(restaurant_turnover = 7) %>%
  dplyr::select(-c(8))

#-----------------FINAL MERGE---------------------------------------

everything <- diseases %>%
  left_join(employed, by = c("the_date_format" = "the_date_format")) %>%
  left_join(travel, by = c("the_date_format" = "the_date_format")) %>%
  left_join(google, by = c("the_date_format" = "the_date_format")) %>%
  left_join(weather, by = c("the_date_format" = "the_date_format")) %>%
  left_join(sales, by = c("the_date_format" = "the_date_format")) %>%
  drop_na()

if (!exists(keepers)) {
  keepers <- c("keepers", "everything")
} else {
  keepers <- union(keepers, "everything")
}

cleanup_env(keepers)
