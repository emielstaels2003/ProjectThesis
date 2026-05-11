#keep this file short, nothing that reads data

# Packages
install.packages("gt")
install.packages("SnowballC")
install.packages("modelsummary")
install.packages("quantmod")
install.packages("readxl")
if (!require("fixest")) install.packages("fixest")

# Libraries
library(dplyr)
library(gt)
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
library(gt)
library(modelsummary)
library(SnowballC)
library(tidytext)
library(stopwords)
library(stringr)

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

# Lijst van de 34 G-SIB tickers gebaseerd op je tabel
gsib_tickers <- c(
  # Europe (ECB)
  "BNP.PA", "GLE.PA", "ACA.PA", "DBK.DE", "CBK.DE", "SAN.MC", "BBVA.MC", "UCG.MI", "INGA.AS", "NDA-FI.HE",
  # USA (Fed)
  "JPM", "BAC", "C", "GS", "MS", "WFC", "BK", "STT",
  # Canada
  "RY.TO", "TD.TO",
  # Japan
  "8306.T", "8316.T", "8411.T",
  # United Kingdom
  "HSBA.L", "BARC.L", "STAN.L", "NWG.L", "LLOY.L",
  # China
  "601398.SS", "601988.SS", "601288.SS", "601939.SS", "601328.SS",
  # Switzerland
  "UBSG.SW"
)
# Controleer of het er exact 34 zijn
length(gsib_tickers)

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
  "BBCA.JK", "BBNI.JK", "BBTN.JK", "024110.KS", "105560.KS", "055550.KS", "086790.KS", "316140.KS", "SBER.ME", "VTBR.ME", 
  "CBOM.ME", "1180.SR", "1120.SR", "1010.SR", 
  "1060.SR", "1080.SR", "SBK.JO", "ABG.JO", "NED.JO", "CPI.JO", "FSR.JO", 
  "GFNORTEO.MX", "BINBURSAO.MX", "SANMEXB.MX", "AKBNK.IS", "GARAN.IS", 
  "ISCTR.IS", "HALKB.IS", "VAKBN.IS", "UBSG.SW" 
)

supervision_dates <- data.frame(
  bank = c(
    "European Central Bank",      # nog op te zoeken maar normaal vanaf 2014 algemeen geweten: 4/11/2014
    "Board of Governors of the Federal Reserve", # 21/07/2010
    "Reserve Bank of Australia",
    "Bank of Mexico",
    "Bank of Canada",
    "Bank of Japan",
    "Bank of England",
    "Central Bank of Argentina",  #1/1/1990
    "Central Bank of Brazil",     #1/1/2000
    "People's Bank of China",     # niet de central bank! dus geen datum!
    "Reserve Bank of India",      #1/4/2016
    "Bank Indonesia",
    "Bank of Korea",
    "Bank of Russia",             #1/1/2007
    "Saudi Central Bank",         # geen data beschikbaar, nog opzoeken! 11/6/1966 KLOPT!!!!
    "Central Bank of the Republic of Turkey",
    "South African Reserve Bank", # geen data beschikbaar, nog opzoeken! 1/2/1991 MOET HET ZIJN
    "Swiss National Bank"
  ),
  direct_supervisory_power_date = as.Date(c(
    "2014-11-04","2010-07-21",NA,NA,NA,NA,
    "2013-04-01","1990-01-01",
    "2000-01-01", NA,"2016-04-01",NA,NA,
    "2007-01-01","1966-06-11",NA,
    "1991-02-01",NA
  )),
  has_direct_power = c(
    TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE,
    FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE
  )
)

print(supervision_dates)

aantal <- length(tickers)
# Toon het resultaat in de console
print(paste("De lijst bevat", aantal, "tickers."))

# Global settings
EVENT_WINDOW_START <- -1       # trading days
EVENT_WINDOW_EINDE <- 1      # trading days
EST_WINDOW_START<- -250        # trading days
EST_WINDOW_EINDE <- -30        # trading days     

