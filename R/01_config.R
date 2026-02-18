# Packages
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

# Global settings
EVENT_WINDOW <- c(-1, 1)      # trading days
EST_WINDOW   <- c(-250, -30)  # trading days
SEED <- 123

set.seed(SEED)

# Paths
PATH_RAW <- "data"
PATH_PROCESSED <- "data"
PATH_OUTPUTS <- "outputs"
