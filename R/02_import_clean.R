#dir.create(PATH_PROCESSED, showWarnings = FALSE, recursive = TRUE)

#saveRDS(speeches_clean, file.path(PATH_PROCESSED, "speeches_clean.rds"))
#saveRDS(prices_clean,   file.path(PATH_PROCESSED, "prices_clean.rds"))


speeches <- readRDS("~/ProjectThesis/data/CBS_dataset_v1.0.rds")

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

getSymbols("RY", from = "2000-01-01", to = Sys.Date())
chartSeries(RY,
            type = "line",             # lijnplot
            subset = "2000::2025",     # periode
            theme = chartTheme("white"), 
            name = "RY.TO (Adjusted Close Price)",
            TA = NULL)                 # Geen extra indicatoren



Bank_Mapping <- read_excel("data/Bank_Mapping.xlsx")
View(Bank_Mapping)

speeches_cleaned <- speeches_cleaned %>%
  rename_with(~ "Date_Original", matches("^Date$|^date$")) %>%
  mutate(Date_Clean = as.Date(as.character(Date_Original)))

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





# 2. De dataset voorbereiden
# We zorgen dat Bank en CentralBank als 'factors' worden gezien voor de Fixed Effects
analysis_data <- final_event_study_results %>%
  mutate(
    Bank = as.factor(Bank),
    CentralBank = as.factor(CentralBank),
    Year = as.factor(format(Date, "%Y"))
  )


