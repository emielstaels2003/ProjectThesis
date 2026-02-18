#keep this fil short, nothing that reads data


# Packages
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
  "^GDAXI",     # Deutsche Bundesbank (Duitsland)
  "^FTSE",      # Bank of England (VK)
  "^N225",      # Bank of Japan
  "^SSMI",      # Swiss National Bank
  "^GSPTSE",    # Bank of Canada
  "^AXJO",      # Reserve Bank of Australia
  "^BSESN",     # Reserve Bank of India
  "^OMX",       # Sveriges Riksbank (Zweden)
  "^OMXH25",    # Bank of Finland
  "^FCHI",      # Bank of France
  "FTSEMIB.MI", # Bank of Italy
  "^IBEX",      # Bank of Spain
  "^HSI",       # Hong Kong Monetary Authority
  "^STI",       # Monetary Authority of Singapore
  "^KLSE",      # Central Bank of Malaysia
  "000001.SS",  # People's Bank of China
  "^ISEQ",       # Central Bank of Ireland
  "PSEI.PS",
  "RY"   # Bangko Sentral ng Pilipinas (Filipijnen)
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
