#keep this file short, nothing that reads data

# Packages
install.packages("modelsummary")
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
#library(modelsummary)

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
# DE DATASET SPEECHES KLEINER MAKEN DOOR ENKEL DE G20 TE SELECTEREN 


target_banks <- c(
  "European Central Bank",
  "Board of Governors of the Federal Reserve", 
  "Reserve Bank of Australia",
  "Bank of Mexico",
  "Bank of Canada",
  "Bank of Japan",
  "Bank of England",
  "Central Bank of Argentina",
  "Central Bank of Brazil",
  "People's Bank of China",
  "Reserve Bank of India",
  "Bank Indonesia",
  "Bank of Korea",
  "Bank of Russia",
  "Saudi Central Bank",
  "Central Bank of the Republic of Turkey",
  "South African Reserve Bank",
  "Swiss National Bank"
)

tickers <- c(
  "BNP.PA", "GLE.PA", "ACA.PA", "DBK.DE", "CBK.DE", "SAN.MC", "BBVA.MC", 
  "UCG.MI", "INGA.AS", "NDA-FI.HE", "JPM", "BAC", "C", "GS", "MS", "WFC", 
  "BK", "STT", "NTRS", "COF", "PNC", "TFC", "USB", "SCHW", "ALLY", "AXP", 
  "CFG", "FITB", "FCNCA", "HBAN", "KEY", "MTB", "RF", "SYF", 
  "ANZ.AX", "CBA.AX", "NAB.AX", "WBC.AX", "RY.TO", "TD.TO", "BMO.TO", 
  "BNS.TO", "CM.TO", "NA.TO", "8306.T", "8316.T", "8411.T", "8604.T", 
  "HSBA.L", "BARC.L", "STAN.L", "NWG.L", "LLOY.L", "GGAL.BA", "BMA.BA", 
  "BPAT.BA", "BBAR.BA", "ITUB4.SA", "BBDC4.SA", "BBAS3.SA", "SANB11.SA", 
  "601398.SS", "601988.SS", "601288.SS", "601939.SS", "601328.SS", 
  "600036.SS", "601166.SS", "601998.SS", "600000.SS", "601658.SS", 
  "SBIN.NS", "HDFCBANK.NS", "ICICIBANK.NS", "BMRI.JK", "BBRI.JK", 
  "BBCA.JK", "BBNI.JK", "BBTN.JK", "024110.KS", "SBER.ME", "VTBR.ME", 
  "CBOM.ME", "1180.SR", "1120.SR", "1010.SR", 
  "1060.SR", "1080.SR", "SBK.JO", "ABG.JO", "NED.JO", "CPI.JO", "FSR.JO", 
  "GFNORTEO.MX", "GFINBURO.MX", "AKBNK.IS", "GARAN.IS", 
  "ISCTR.IS", "HALKB.IS", "VAKBN.IS", "UBSG.SW"
)
aantal <- length(tickers)
# Toon het resultaat in de console
print(paste("De lijst bevat", aantal, "tickers."))

# Global settings
EVENT_WINDOW_START <- -1       # trading days
EVENT_WINDOW_EINDE <- 1        # trading days
EST_WINDOW_START<- -250        # trading days
EST_WINDOW_EINDE <- -30        # trading days     

