#keep this file short, nothing that reads data

# Packages
install.packages("quantmod")
if (!require("fixest")) install.packages("fixest")

# Libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(quantmod)
library(tidyverse)
library(stringr)
library(readxl)
library(tidyverse)
library(quantmod)
library(lubridate)
library(fixest)
library(broom)


market_indices <- c(
  "^GSPC",      # US Federal Reserve
  "^STOXX50E",  # European Central Bank (Eurozone benchmark)
  "^FTSE",      # Bank of England (VK)
  "^N225",      # Bank of Japan
  "^SSMI",      # Swiss National Bank
  "^GSPTSE",    # Bank of Canada
  "^AXJO",      # Reserve Bank of Australia
  "^BSESN",     # Reserve Bank of India
  "000001.SS",  # People's Bank of China
  "^MXX",       # Bank of Mexico
  "^MERV",      # Central Bank of Argentina
  "^BVSP",      # Central Bank of Brazil
  "^JKSE",      # Bank Indonesia
  "^KS11",      # Bank of Korea
  "IMOEX.ME",   # Bank of Russia
  "KSA",      # Saudi Central Bank
  "XU100.IS",   # Central Bank of Turkey
  "^J203.JO"    # South African Reserve Bank
)


# Global settings
EVENT_WINDOW <- c(-1, 1)      # trading days
EST_WINDOW   <- c(-250, -30)  # trading days
SEED <- 123

set.seed(SEED)

# Paths
PATH_RAW <- "data"
PATH_PROCESSED <- "data"
PATH_OUTPUTS <- "outputs"
