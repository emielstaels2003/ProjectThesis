install.packages("quantmod")
library(quantmod)

#INLADEN DATASET SPEECHES

speeches <- readRDS("CBS_dataset_v1.0.rds")
#https://cbspeeches.com/
cb_counts <- table(speeches$CentralBank)
cb_matrix <- cbind(CentralBank = names(cb_counts),
                   Observations = as.numeric(cb_counts))
cb_matrix



#TWEE GROEPEN MAKEN VOOR DE FED: BOARD EN REGIONAL BANKS

speeches_cleaned <- speeches
#Maak een index voor de "Board of Governors"
board_indices <- grepl("Board of Governors", speeches_cleaned$CentralBank)
#Maak een index voor de regionale banken 
# (We zoeken naar "Federal Reserve Bank of" om bijv. New York, St. Louis, etc. te pakken)
regional_indices <- grepl("Federal Reserve Bank of", speeches_cleaned$CentralBank)
#Overschrijf de namen in de dataset
speeches_cleaned$CentralBank[board_indices] <- "Fed: Board of Governors"
speeches_cleaned$CentralBank[regional_indices] <- "Fed: Regional Banks"
#Nieuwe tabel maken met de gesplitste counts
cb_counts_new <- table(speeches_cleaned$CentralBank)
#Filteren op de drempelwaarde van 500 observaties
cb_counts_filtered <- cb_counts_new[cb_counts_new > 500]
#De definitieve matrix maken
cb_matrix_final <- cbind(CentralBank = names(cb_counts_filtered), 
                         Observations = as.numeric(cb_counts_filtered))
#Optioneel: Sorteren op aantal observaties voor een beter overzicht
cb_matrix_final <- cb_matrix_final[order(as.numeric(cb_matrix_final[,2]), decreasing = TRUE), ]
#Resultaat bekijken
print(cb_matrix_final)



#MARKET INDICES GEKOPPELD AAN CENTRALE BANKEN

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



#INLADEN DATASET STOCK PRICES EN OMVORMEN NAAR LOG(RETURN) DATA

#Download de data naar een lijst (auto.assign = FALSE is cruciaal hier)
market_data_list <- lapply(market_indices, function(x) {
  message(paste("Bezig met downloaden van:", x))
  getSymbols(x, src = "yahoo", from = "1986-01-01", to = "2023-11-30", auto.assign = FALSE)
})
#Geef de lijst-items de namen van de tickers voor de herkenbaarheid
names(market_data_list) <- market_indices
#Haal de Adjusted prijzen op en voeg ze samen in één tabel (xts object)
# We gebruiken Ad() om de Adjusted Close kolom te pakken
market_prices <- do.call(merge, lapply(market_data_list, Ad))
#Hernoem de kolommen naar de originele tickers (verwijder ".Adjusted" uit de namen)
colnames(market_prices) <- market_indices
#Vul ontbrekende waarden in (voor feestdagen en tijdzone-verschillen)
#na.locf = Last Observation Carried Forward
market_prices_clean <- na.locf(market_prices, na.rm = FALSE)
#Bereken de dagelijkse rendementen (Returns)
# Dit is meestal wat je gebruikt in regressies of analyses
market_returns <- diff(log(market_prices_clean))



# TEST VOOR STOCK PRICE DATA: GRAFIEK MAKEN VAN STOCK PRICE

getSymbols("^STOXX50E", from = "2000-01-01", to = Sys.Date())
chartSeries(STOXX50E,
            type = "line",             # lijnplot
            subset = "2000::2025",     # periode
            theme = chartTheme("white"), 
            name = "STOXX50E (Adjusted Close Price)",
            TA = NULL)                 # Geen extra indicatoren



#CATEGORISATIE EN SCORING

library(tidyverse)
library(stringr)
library(dplyr)
#Definieer de zoektermen (Dictionaries)
tight_terms <- "interest rate|inflation|tightening|hawkish|price stability|monetary policy"
reg_terms   <- "capital requirement|basel|liquidity|regulatory|solvency|macroprudential"
sup_terms   <- "oversight|monitoring|inspection|stress test|supervisory|enforcement"
#Bereken de Raw Scores (Ratio van trefwoorden t.o.v. totale tekst)
# We doen dit op de volledige dataset
speeches_cleaned <- speeches_cleaned %>%
  mutate(
    word_count = str_count(text, "\\w+"),
    raw_T = str_count(tolower(text), tight_terms) / word_count,
    raw_R = str_count(tolower(text), reg_terms) / word_count,
    raw_S = str_count(tolower(text), sup_terms) / word_count
  )
#Functie om scores om te zetten naar -1, 0, 1 op basis van intervallen
# Let op: we behandelen 0 als een aparte groep
assign_trichotomous_score <- function(raw_vector) {
  final_scores <- rep(0, length(raw_vector)) # Start alles op 0
  # Alleen de speeches die het onderwerp wél noemen (score > 0) verdelen we, speeches die het onderwerp niet noemen worden gewoon op 0 gezet
  positive_indices <- which(raw_vector > 0)
  positive_values  <- raw_vector[positive_indices]
  if(length(positive_values) > 0) {
    # We verdelen de positieve waarden in 3 groepen (tercielen)
    # ntile geeft 1, 2 of 3
    bins <- ntile(positive_values, 3)
    # We mappen dit naar jouw gewenste schaal:
    # De laagste 33% van de positieve scores wordt -1 (weinig intens)
    # De middelste 33% wordt 0 (gemiddeld)
    # De hoogste 33% wordt +1 (zeer intens)
    final_scores[positive_indices] <- bins - 2
  }
  return(final_scores)
}
#De extra kolommen aanmaken in de dataset
speeches_cleaned <- speeches_cleaned %>%
  mutate(
    Tightness   = assign_trichotomous_score(raw_T),
    Regulation  = assign_trichotomous_score(raw_R),
    Supervision = assign_trichotomous_score(raw_S)
  )
#Controleer de verdeling
print("Verdeling van de scores:")
table(speeches_cleaned$Tightness)
table(speeches_cleaned$Supervision)
table(speeches_cleaned$Regulation)
