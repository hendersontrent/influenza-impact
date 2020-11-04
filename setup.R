#----------------------------------------
# This script sets out to load all 
# things required for the project
#----------------------------------------

#----------------------------------------
# Author: Trent Henderson, 3 August 2020
#----------------------------------------

# Load packages

library(tidyverse)
library(readxl)
library(scales)
library(forecast)
library(ggpubr)
library(Cairo)
library(tseries)
library(urca)
library(janitor)
library(data.table)
library(viridis)
library(zoo)
library(CausalImpact)
library(lubridate)
library(mgcv)

# Turn off scientific notation

options(scipen = 999)

# Load functions that are useful

keepers <- c("keepers")

r_files <- list.files("R", full.names = TRUE, pattern = "\\.[Rr]")
for(f in r_files){
  source(f)
}

if (!exists(keepers)) {
  keepers <- c("keepers", "cleanup_env")
} else {
  keepers <- union(keepers, "cleanup_env")
}

if (!exists(keepers)) {
  keepers <- c("keepers", "cleaner")
} else {
  keepers <- union(keepers, "cleaner")
}

# Load data and process it

r_files_1 <- list.files("processing", full.names = TRUE, pattern = "\\.[Rr]")
for(j in r_files_1){
  source(j)
}

# Create an output folder if none exists

if(!dir.exists('output')) dir.create('output')
