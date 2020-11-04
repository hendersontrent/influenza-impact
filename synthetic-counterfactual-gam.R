#----------------------------------------------------------
# This script sets out to produce a GAM counterfactual
# forecast
#
# NOTE: This script requires setup.R to have been run first
#----------------------------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 4 November 2020
#-----------------------------------------

#------------------------ MODELLING --------------------------------

# Remove post-intervention period data

months <- c(5,6,7,8,9,10)

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

# Make some new data that is 2019-2021

newdat <- data.frame(nyear = c(2020,2020,2020,2020,2020,2020,2020,2020),
                     nmonth = c(5,6,7,8,9,10,11,12))

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

#------------------------ PLOTTING ---------------------------------

# Render plots

p <- gam_dat %>%
  ggplot() +
  geom_ribbon(data = preds, aes(x = date, ymin = lower, ymax = upper), fill = "steelblue2", alpha = 0.4) +
  geom_line(data = preds, aes(x = date, y = fitted), colour = "steelblue2", size = 1, linetype = "dashed") +
  geom_line(aes(x = the_date_format, y = flu), colour = "#05445E", size = 1.25) +
  labs(title = "Forecasted counterfactual influenza rate vs actual reported influenza rate on log scale",
       subtitle = paste0("Dark blue line indicates actual data. Light blue indicates model-generated counterfactual forecast.\n",
                         "Assumes COVID-19 impacts are evident from April 2020.\n",
                         the_text),
       x = "Date",
       y = "log(influenza rate)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
print(p)

p1 <- gam_dat %>%
  ggplot() +
  geom_ribbon(data = preds, aes(x = date, ymin = exp(lower), ymax = exp(upper)), fill = "steelblue2", alpha = 0.4) +
  geom_line(data = preds, aes(x = date, y = exp(fitted)), colour = "steelblue2", size = 1, linetype = "dashed") +
  geom_line(aes(x = the_date_format, y = exp(flu)), colour = "#05445E", size = 1.25) +
  labs(title = "Forecasted counterfactual model on original scale",
       x = "Date",
       y = "Influenza rate per 100,000 persons",
       caption = "Analysis: Orbisant Analytics.\nModel is a generalised additive model with annual trend and monthly seasonality terms.") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
print(p1)

#------------------------ EXPORTS ----------------------------------

CairoPNG("output/synthetic-counterfactual-forecast.png", 800, 600)
ggarrange(p, p1, ncol = 1, nrow = 2)
dev.off()
