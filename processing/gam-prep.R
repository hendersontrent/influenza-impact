#----------------------------------------
# This script sets out to load and prep
# the data for a a GAM forecast
#----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 4 November 2020
#-----------------------------------------

# Filepaths

flupath_gam <- "data/flu.xlsx"

# Cleaning

flu_gam <- cleaner(flupath_gam) %>%
  rename(flu = value)

the_av_gam <- flu_gam %>%
  filter(the_date_format == as.Date("2002-04-01") | the_date_format == as.Date("2003-04-01")) %>%
  mutate(category = 1) %>%
  group_by(category) %>%
  summarise(av = mean(flu)) %>%
  ungroup()

the_val_gam <- as.vector(the_av_gam$av) 

gam_dat <- flu_gam %>%
  mutate(flu = case_when(
    the_date_format == as.Date("2001-04-01") ~ the_val_gam,
    TRUE                                     ~ flu)) %>% 
  mutate(nyear = gsub("-.*", "\\1", the_date_format),
         nmonth = gsub('.{3}$', '', the_date_format),
         nmonth = gsub('^.{5}', '', nmonth),
         nyear = as.numeric(nyear),
         nmonth = as.numeric(nmonth))

# Retain

if (!exists(keepers)) {
  keepers <- c("keepers", "gam_dat")
} else {
  keepers <- union(keepers, "gam_dat")
}

cleanup_env(keepers)
