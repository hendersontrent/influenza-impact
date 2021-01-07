#---------------------------------------
# This script sets out to produce final
# analysis of distributions and forecasts
# of the influenza rate for Australia
#
# NOTE: This script requires setup.R to
# have been run first
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson 7 January 2021
#---------------------------------------

#' Function for cleaning time series data
#' 
#' @param filepath quoted valid filepath to dataset
#' @returns a cleaned dataframe of 2 columns [date, log-scaled flu rate] ready for analysis
#' @author Trent Henderson

cleaner <- function(filepath){
  
  d <- read_excel(filepath) %>%
    gather(key = month, value = value, 2:13)
  
  d1 <- d %>%
    mutate(the_date = paste0("01-",month,"-",year)) %>%
    mutate(the_date_format = as.Date(the_date, "%d-%b-%Y")) %>%
    mutate(value = log(value)) %>%
    mutate(value = case_when(
      value == -Inf ~ log(min(value)), # Need a better imputation method than this - log scaling returns NaNs for 0 rates
      TRUE          ~ value)) %>%
    dplyr::select(c(the_date_format, value))
  
  return(d1)
}

# Clean it

flupath <- "data/flu.xlsx"

flu_gam <- cleaner(flupath) %>%
  rename(flu = value)

the_av_gam <- flu_gam %>%
  filter(the_date_format == as.Date("2002-04-01") | the_date_format == as.Date("2003-04-01")) %>%
  mutate(category = 1) %>%
  group_by(category) %>%
  summarise(av = mean(flu)) %>%
  ungroup()

the_val_gam <- as.vector(the_av_gam$av) 

# Get months and years as separate numeric columns

gam_dat <- flu_gam %>%
  mutate(flu = case_when(
    the_date_format == as.Date("2001-04-01") ~ the_val_gam,
    TRUE                                     ~ flu)) %>% 
  mutate(nyear = gsub("-.*", "\\1", the_date_format),
         nmonth = gsub('.{3}$', '', the_date_format),
         nmonth = gsub('^.{5}', '', nmonth),
         nyear = as.numeric(nyear),
         nmonth = as.numeric(nmonth))

#-------------------------- Distributions --------------------------

p <- gam_dat %>%
  mutate(category = ifelse(the_date_format < as.Date("2020-04-01", format = "%Y-%m-%d"), "Pre-COVID (Jan 2001-March 2020)", "COVID")) %>%
  mutate(category = factor(category, levels = c("Pre-COVID (Jan 2001-March 2020)", "COVID"))) %>%
  ggplot() +
  geom_density(aes(fill = category, x = flu), alpha = 0.5) +
  labs(title = "Density of Australia's influenza notification rate per 100k persons",
       subtitle = "COVID distribution is comprised of April 2020-December 2020 inclusive.",
       x = "log(influenza rate)",
       y = "Density",
       fill = NULL) +
  scale_x_continuous(limits = c(-4,7)) +
  scale_fill_manual(values = c("steelblue2", "#FD49A0")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")
print(p)

#-------------------------- Modelling ------------------------------

# Remove post-intervention period data

months <- c(4,5,6,7,8,9,10,11,12)

truncated <- gam_dat %>%
  mutate(indicator = case_when(
    nyear == 2020 & nmonth %in% months ~ "Remove",
    TRUE                               ~ "Keep")) %>% 
  filter(indicator == "Keep")

# Model spec

m1 <- gam(flu ~ s(nmonth, k = 12, bs = "cc") + s(nyear, k = 18),
          data = truncated,
          method = "REML")

the_text <- paste0("Model deviance explained = ", round(summary(m1)$dev.expl*100, digits = 2), "%")

# Make some new data that is end of 2020

newdat <- data.frame(nyear = c(2020,2020,2020,2020,2020,2020,2020,2020,2020),
                     nmonth = c(4,5,6,7,8,9,10,11,12))

# Predict counts based on model-smoothed seasonality and trend
# and account for link function inverse

ilink <- family(m1)$linkinv

newd <- cbind(newdat, as.data.frame(predict(m1, newdat, type = "link", se.fit = TRUE)))
newd <- transform(newd, fitted = ilink(fit), upper = ilink(fit + (2 * se.fit)),
                  lower = ilink(fit - (2 * se.fit)))

preds <- newd %>%
  mutate(nmonth_adj = case_when(
    nmonth < 10 ~ paste0("0",nmonth),
    TRUE        ~ as.character(nmonth))) %>% 
  mutate(date = paste0(nyear,"-",nmonth_adj,"-01")) %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

#---------------
# MODEL PLOTTING
#---------------

p1 <- gam_dat %>%
  mutate(grouping = ifelse(the_date_format < as.Date("2020-04-01", format = "%Y-%m-%d"), "Actual data: Pre-COVID impacts", "Actual data: COVID impacts visible")) %>%
  mutate(grouping = factor(grouping, levels = c("Actual data: Pre-COVID impacts", "Actual data: COVID impacts visible"))) %>%
  ggplot() +
  geom_ribbon(data = preds, aes(x = date, ymin = lower, ymax = upper), fill = "steelblue2", alpha = 0.4) +
  geom_line(data = preds, aes(x = date, y = fitted), colour = "steelblue2", size = 1, linetype = "dashed") +
  geom_line(aes(x = the_date_format, y = flu, colour = grouping), size = 1) +
  annotate(geom = "rect", xmin = as.Date(min(gam_dat$the_date_format)), xmax = as.Date("2020-03-31", format = "%Y-%m-%d"), 
           ymax = Inf, ymin = -Inf, fill = "#DCD2CC", alpha = 0.4) +
  labs(title = "Forecasted Australian influenza rate vs actual lab reported influenza rate on log scale",
       subtitle = paste0("Dark blue and pink lines indicate actual data. Light blue line/shading indicates model-generated forecast and 95% confidence interval. Assumes COVID-19 impacts are evident from April 2020.\n",
                         the_text),
       x = "Date",
       y = "log(influenza rate)",
       caption = "Basic forecasting performed using a generalised additive model with cyclic cubic spline for monthly seasonality and smooth between-year trend term.",
       colour = NULL) +
  scale_colour_manual(values = c("#05445E", "#FD49A0")) +
  scale_x_date(expand = c(0,1), date_breaks = "1 year", date_labels = "%Y") + # adjust aesthetics as necessary
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")
print(p1)

#------------------------ EXPORTS ---------------------------------

# Density

CairoPNG("output/density.png", 800, 600)
print(p)
dev.off()

# Forecast

CairoPNG("output/flu-forecast.png", 800, 600)
# Need to re-do subtitle to fit on auto-rescaled webpage image widget
p2 <- p1 +
  labs(subtitle = paste0("Dark blue and pink lines indicate actual data. Light blue line/shading indicates model-generated forecast and 95% confidence interval.\nAssumes COVID-19 impacts are evident from April 2020.\n",
                         the_text))
print(p2)
dev.off()

# Both for cover image

CairoPNG("output/merged.png", 1100, 1100)
ggarrange(p, p1, ncol = 1, nrow = 2)
dev.off()
