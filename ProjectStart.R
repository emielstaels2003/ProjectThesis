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

# Market Indices gekoppeld aan Centrale Banken
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
  "PSEI.PS"    # Bangko Sentral ng Pilipinas (Filipijnen)
)

# 2. Download de data naar een lijst (auto.assign = FALSE is cruciaal hier)
market_data_list <- lapply(market_indices, function(x) {
  message(paste("Bezig met downloaden van:", x))
  getSymbols(x, src = "yahoo", from = "1986-01-01", to = "2023-11-30", auto.assign = FALSE)
})

# 3. Geef de lijst-items de namen van de tickers voor de herkenbaarheid
names(market_data_list) <- market_indices

# 4. Haal de Adjusted prijzen op en voeg ze samen in één tabel (xts object)
# We gebruiken Ad() om de Adjusted Close kolom te pakken
market_prices <- do.call(merge, lapply(market_data_list, Ad))

# 5. Hernoem de kolommen naar de originele tickers (verwijder ".Adjusted" uit de namen)
colnames(market_prices) <- market_indices

# 6. Vul ontbrekende waarden in (voor feestdagen en tijdzone-verschillen)
# na.locf = Last Observation Carried Forward
market_prices_clean <- na.locf(market_prices, na.rm = FALSE)

# 7. (Optioneel) Bereken de dagelijkse rendementen (Returns)
# Dit is meestal wat je gebruikt in regressies of analyses
market_returns <- diff(log(market_prices_clean))

# Bekijk het resultaat
head(market_prices_clean)
summary(market_returns)

#Maak een mooie grafiek met de Adjusted Close-prijs
getSymbols("^ISEQ", from = "2000-01-01", to = Sys.Date())
chartSeries(ISEQ,
            type = "line",             # lijnplot
            subset = "2000::2025",     # periode
            theme = chartTheme("white"), 
            name = "ISEQ (Adjusted Close Price)",
            TA = NULL)                 # Geen extra indicatoren

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

