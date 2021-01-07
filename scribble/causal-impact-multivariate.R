#----------------------------------------------------------
# This script sets out to produce a Bayesian structural 
# time series causal impact assessment
#
# NOTE: This script requires setup.R to have been run first
#----------------------------------------------------------

#---------------------------------------
# Author: Trent Henderson, 5 August 2020
#---------------------------------------

my.zoo <- everything %>% 
  dplyr::select(c(1,2,7,8)) %>% # Just variables that correlate with influenza
  filter(the_date_format < as.Date("2020-09-01")) %>%
  read.zoo()

pre.period <- as.Date(c("2004-01-01", "2020-03-01"))
post.period <- as.Date(c("2020-04-01", "2020-08-01"))

impact <- CausalImpact(data = my.zoo, 
                       pre.period = pre.period,
                       post.period = post.period,
                       alpha = 0.1,
                       model.args = list(niter = 3000,
                                         nseasons = 12,
                                         season.duration = 1,
                                         prior.level.sd = 0.01))

summary(impact)
plot(impact, c("original", "pointwise"))
summary(impact, "report")

# Posterior probability of predictors being included in the model

plot(impact$model$bsts.model, "coefficients")

the_coefs <- as.data.frame(impact$model$bsts.model$coefficients)

#---------------------DATA VISUALISATION----------------------------

prob_of_impact <- round((1-impact$summary$p[1])*100, digits = 2)

impact_plot <- plot(impact, c("original", "pointwise")) +
  labs(title = "Estimated causal impact of COVID-19 restrictions on Australia's influenza rate",
       subtitle = paste0("Posterior probability of causal effect = ", prob_of_impact,"%. Assumes restriction effects visible from Apr 2020.\nBlack line indicates actual log(influenza rate). Blue line indicates synthetic Bayesian counterfactual."),
       x = "Date",
       y = NULL,
       caption = "Covariates: Mean Australian temperature and flu vaccine Google search trends.") +
  theme_bw()
print(impact_plot)

#---------------------EXPORT----------------------------------------

CairoPNG("output/causal-impact-multivariate.png", 700, 450)
print(impact_plot)
dev.off()
