x=1

install.packages("quantmod")
library(quantmod)

#INLADEN DATASET SPEECHES
speeches <- readRDS("CBS_dataset_v1.0.rds")
#https://cbspeeches.com/
cb_counts <- table(speeches$CentralBank)
cb_matrix <- cbind(CentralBank = names(cb_counts),
                   Observations = as.numeric(cb_counts))
cb_matrix

speeches_cleaned <- speeches
fed_indices <- grepl("Federal Reserve|Board of Governors", speeches_cleaned$CentralBank)
speeches_cleaned$CentralBank[fed_indices] <- "US Federal Reserve System"

# 3. Nieuwe tabel maken met de samengevoegde counts
cb_counts_new <- table(speeches_cleaned$CentralBank)

# 4. Filteren op de drempelwaarde van 500 observaties
cb_counts_filtered <- cb_counts_new[cb_counts_new > 500]

# 5. De definitieve matrix maken
cb_matrix_final <- cbind(CentralBank = names(cb_counts_filtered), 
                         Observations = as.numeric(cb_counts_filtered))

# Optioneel: Sorteren op aantal observaties voor een beter overzicht
cb_matrix_final <- cb_matrix_final[order(as.numeric(cb_matrix_final[,2]), decreasing = TRUE), ]

# Resultaat bekijken
print(cb_matrix_final)

# voorstel om alle verschillende Fed te clusteren

#INLEZEN EN MAKEN VAN DATASET PRICES
tickers <- c(
  # United States
  "JPM", "BAC", "C", "GS", "MS", "WFC", "USB", "PNC", "COF", "TFC",
  
  # United Kingdom
  "HSBA.L", "BARC.L", "LLOY.L", "NWG.L", "STAN.L",
  
  # Eurozone
  "BNP.PA", "GLE.PA", "ACA.PA", "DBK.DE", "CBK.DE", "INGA.AS", "KBC.BR",
  "UCG.MI", "ISP.MI", "SAN.MC", "BBVA.MC", "NDA-SE.ST", "SEB-A.ST", "DANSKE.CO",
  
  # Switzerland
  "UBSG.SW",
  
  # Japan
  "8306.T", "8316.T", "8411.T",
  
  # China
  "1398.HK", "0939.HK", "3988.HK", "1288.HK",
  
  # Singapore
  "D05.SI", "O39.SI", "U11.SI",
  
  # India
  "HDFCBANK.NS", "ICICIBANK.NS", "SBIN.NS", "AXISBANK.NS",
  
  # Canada
  "RY.TO", "TD.TO", "BNS.TO", "BMO.TO", "CM.TO",
  
  # Australia
  "CBA.AX", "WBC.AX", "ANZ.AX", "NAB.AX",
  
  # Nordics and others
  "DNB.OL", "SWED-A.ST", "SHB-A.ST",
  
  # Emerging markets
  "ITUB", "BBD", "GFNORTEO.MX", "SBK.JO", "ABG.JO", "0011.HK", "0023.HK"
)

tickers1 <- c(
  # Globale Indexen
  "^GSPC",      # S&P 500 Index (Globale/VS Markt Benchmark)
  "^MSCI",      # MSCI World Index (Globale Markt Benchmark)
  
  # Regionale Financiële Indexen (Proxies voor Afhankelijke Variabele R_it)
  "^FCHI",      # CAC 40 Index (Proxy Eurozone - stabiel op Yahoo)
  "^FTSE",      # FTSE 100 Index (Proxy VK Financiële Sector)
  "^N225",      # Nikkei 225 Index (Proxy Japanse Markt)
  "^OMXC25",    # OMX Kopenhagen (Proxy Scandinavië/Noorden)
  "^SSMI",      # Swiss Market Index (Proxy Zwitserse Markt)
  
  # Individuele SIFIs (Voor Bèta-Analyse)
  "JPM",        # JPMorgan Chase (VS)
  "BNP.PA",     # BNP Paribas (Eurozone - Parijs)
  "HSBA.L",     # HSBC Holdings (VK - Londen)
  "UBSG.SW"     # UBS Group (Zwitserland - Zürich)
)

# Test de nieuwe lijst
prices1 <- getSymbols(tickers1, 
                               src = "yahoo", 
                               from = "1986-01-01", 
                               to = "2023-11-30", 
                               auto.assign = TRUE)

getSymbols(tickers1, from = "1986-01-01", to = "2023-11-30")
prices <- do.call(merge, lapply(tickers, function(t) Ad(get(t))))
colnames(prices) <- tickers


#Maak een mooie grafiek met de Adjusted Close-prijs
getSymbols("KBC.BR", from = "2000-01-01", to = Sys.Date())
chartSeries(KBC.BR,
            type = "line",             # lijnplot
            subset = "2000::2025",     # periode
            theme = chartTheme("white"), 
            name = "KBC Group (Adjusted Close Price)",
            TA = NULL)                 # Geen extra indicatoren


#DATASET PRICES OMVORMEN NAAR EEN DATASET RETURNS 
library(tidyverse)
prices <- data.frame(date = index(prices), coredata(prices))
# Zorg dat 'date' een Date-kolom is
prices <- prices %>% arrange(date)

#Functie die voor één kolom de return berekent

calc_returns <- function(prices_vec) {
  # Vervang NA's tijdelijk door de laatst bekende waarde (carry forward)
  filled <- zoo::na.locf(prices_vec, na.rm = FALSE)  # 'Last Observation Carried Forward'
  
  # Bereken return: (p_t - p_{t-1}) / p_{t-1}
  returns <- (filled / dplyr::lag(filled)) - 1
  
  # Als de huidige prijs NA is, zet return ook op NA
  returns[is.na(prices_vec)] <- NA
  
  return(returns)
}

#Pas dit toe op alle banken tegelijk
library(zoo)  # voor na.locf()
returns <- prices %>%
  mutate(across(-date, calc_returns))

