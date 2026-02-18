h
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
  "PSEI.PS",
  "RY"   # Bangko Sentral ng Pilipinas (Filipijnen)
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

getSymbols("RY", from = "2000-01-01", to = Sys.Date())
chartSeries(RY,
            type = "line",             # lijnplot
            subset = "2000::2025",     # periode
            theme = chartTheme("white"), 
            name = "RY.TO (Adjusted Close Price)",
            TA = NULL)                 # Geen extra indicatoren



#CATEGORISATIE EN SCORING

library(tidyverse)
library(stringr)
#de functie assign_trichotomous_score definiëren omdat we dit nodig zullen hebben om in groepen te verdelen
assign_trichotomous_score <- function(raw_vector) {
  final_scores <- rep(0, length(raw_vector)) 
  positive_indices <- which(raw_vector > 0)
  positive_values  <- raw_vector[positive_indices]
  
  if(length(positive_values) > 0) {
    bins <- ntile(positive_values, 3)
    final_scores[positive_indices] <- bins - 2
  }
  return(final_scores)
}
#Definieer de verfijnde trefwoordenlijsten
# Voor Tightness gebruiken we de Hawkish (+) en Dovish (-) verdeling
hawkish_terms <- "\\b(increase|raise|higher|tightening|hawkish|restrictive|upside risk|tapering|hike|above target)\\b"
dovish_terms  <- "\\b(decrease|lower|cut|easing|dovish|accommodative|stimulus|downside risk|supportive|below target)\\b"
#Voor Regulation en Supervision blijven we bij de 'intensiteit' trefwoorden
reg_terms <- "\\b(basel|solvency|regulatory framework|capital requirement|leverage ratio|risk-weighting|own funds|capital buffer|liquidity ratio|lcr|nsfr|liquidity coverage|macroprudential|systemic risk|crr|crd)\\b"
sup_terms <- "\\b(oversight|monitoring|inspection|examination|enforcement|sanctions|remedial action|early intervention|stress test|srep|audit|supervisory review|on-site|reporting obligations|disclosure|compliance)\\b"
#Bereken de scores
speeches_cleaned <- speeches_cleaned %>%
  mutate(
    # Voorbereiding: tekst naar kleine letters en woordenaantal
    text_low = tolower(text),
    word_count = str_count(text, "\\w+"),
    
    # A. TIGHTNESS: IMF Sentiment Formule (Netto Sentiment)
    pos_count = str_count(text_low, hawkish_terms),
    neg_count = str_count(text_low, dovish_terms),
    # De 0.0001 voorkomt een 'division by zero'
    raw_T_sentiment = (pos_count - neg_count) / (pos_count + neg_count + 0.0001),
    
    # B. REGULATION & SUPERVISION: Ruwe intensiteit (zoals voorheen)
    raw_R = str_count(text_low, reg_terms) / word_count,
    raw_S = str_count(text_low, sup_terms) / word_count
  )
#Omzetten naar finale variabelen
# Voor Tightness gebruiken we de continue Z-score (nauwkeuriger voor regressie)
speeches_cleaned <- speeches_cleaned %>%
  mutate(
    Tightness = (raw_T_sentiment - mean(raw_T_sentiment, na.rm=TRUE)) / sd(raw_T_sentiment, na.rm=TRUE)
  )

#Voor Regulation en Supervision behouden we de -1, 0, 1 intervallen (robuustheid)
# (Gebruik de 'assign_trichotomous_score' functie die je al in je script had staan)
speeches_cleaned <- speeches_cleaned %>%
  mutate(
    Regulation  = assign_trichotomous_score(raw_R),
    Supervision = assign_trichotomous_score(raw_S)
  )
#Controleer het resultaat
print("Samenvatting Tightness (Z-score):")
summary(speeches_cleaned$Tightness)
print("Verdeling Regulation & Supervision:")
table(speeches_cleaned$Regulation)
table(speeches_cleaned$Supervision)



#VERSCHIL IN REGULATION EN SUPERVISION VOOR FED BOARD EN FED REGIONALS (EERDER ZIEN ALS EEN OBSERVATIE)

library(tidyverse)# 1. Gemiddelde scores berekenen per groepfed_vergelijking <- speeches_cleaned %>%filter(CentralBank %in% c("Fed: Regional Banks", "Fed: Board of Governors")) %>%group_by(CentralBank) %>%summarise(Gemiddelde_Regulation = mean(Regulation, na.rm = TRUE),Gemiddelde_Supervision = mean(Supervision, na.rm = TRUE),Aantal_Speeches = n())# 2. Resultaat printen in je consoleprint(fed_vergelijking)# 3. Grafiek maken (om het focusverschil te zien)fed_long <- fed_vergelijking %>%pivot_longer(cols = c(Gemiddelde_Regulation, Gemiddelde_Supervision),names_to = "Thema", values_to = "Score")ggplot(fed_long, aes(x = CentralBank, y = Score, fill = Thema)) +geom_bar(stat = "identity", position = "dodge") +theme_minimal() +labs(title = "Focusverschil: Board vs Regional Banks", y = "Score", x = "Groep")# 4. Statistische toets (T-test)t_test_reg <- t.test(Regulation ~ CentralBank, data = filter(speeches_cleaned, CentralBank %in% c("Fed: Regional Banks", "Fed: Board of Governors")))t_test_sup <- t.test(Supervision ~ CentralBank, data = filter(speeches_cleaned, CentralBank %in% c("Fed: Regional Banks", "Fed: Board of Governors")))# 5. P-waarden tonen (lager dan 0.05 is significant)print("P-waarde Regulation:")print(t_test_reg$p.value)
print("P-waarde Supervision:")
print(t_test_sup$p.value)

fed_vergelijking <- speeches_cleaned %>%
  filter(CentralBank %in% c("Fed: Regional Banks", "Fed: Board of Governors")) %>%
  group_by(CentralBank) %>%
  summarise(
    Gemiddelde_Regulation = mean(Regulation, na.rm = TRUE),
    Gemiddelde_Supervision = mean(Supervision, na.rm = TRUE),
    Aantal_Speeches = n()
  )
print(fed_vergelijking)
fed_long <- fed_vergelijking %>%
  pivot_longer(cols = c(Gemiddelde_Regulation, Gemiddelde_Supervision),
               names_to = "Thema", values_to = "Score")

ggplot(fed_long, aes(x = CentralBank, y = Score, fill = Thema)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Focusverschil: Board vs Regional Banks", y = "Score", x = "Groep")
t_test_reg <- t.test(Regulation ~ CentralBank, data = filter(speeches_cleaned, CentralBank %in% c("Fed: Regional Banks", "Fed: Board of Governors")))
t_test_sup <- t.test(Supervision ~ CentralBank, data = filter(speeches_cleaned, CentralBank %in% c("Fed: Regional Banks", "Fed: Board of Governors")))
print("P-waarde Regulation:")
print(t_test_reg$p.value)
print("P-waarde Supervision:")
print(t_test_sup$p.value)



#IMPORTEREN VAN BANK DATASET MAPPING

library(readxl)
Bank_Mapping <- read_excel("C:/Users/emiel/OneDrive - KU Leuven/Documents/THESIS/Selectie Banken/Bank_Mapping.xlsx")
View(Bank_Mapping)

library(tidyverse)
library(quantmod)
library(lubridate)

# --- STAP 0: DATUM & KOLOMNAAM REPARATIE ---
# We zorgen dat de kolom altijd 'Date_Clean' heet om verwarring te voorkomen
speeches_cleaned <- speeches_cleaned %>%
  rename_with(~ "Date_Original", matches("^Date$|^date$")) %>%
  mutate(Date_Clean = as.Date(as.character(Date_Original)))

# Controleer of dit gewerkt heeft
print("Check datums in speeches dataset:")
print(head(speeches_cleaned$Date_Clean))

# --- STAP 1: INITIALISATIE ---
results_list <- list()
counter <- 1

# --- STAP 2: DE SYSTEMATISCHE LOOP ---
for(j in 1:nrow(Bank_Mapping)) {
  
  current_bank       <- Bank_Mapping$Ticker[j]
  current_index      <- Bank_Mapping$Index_Ticker[j]
  relevant_cb_name   <- Bank_Mapping$CentralBank[j]
  
  message(paste0("--- ANALYSE START: ", current_bank, " (", relevant_cb_name, ") ---"))
  
  # 1. Haal beursdata op
  b_data <- try(getSymbols(current_bank, src = "yahoo", from = "1986-01-01", 
                           to = "2023-11-30", auto.assign = FALSE), silent = TRUE)
  i_data <- try(getSymbols(current_index, src = "yahoo", from = "1986-01-01", 
                           to = "2023-11-30", auto.assign = FALSE), silent = TRUE)
  
  if(inherits(b_data, "try-error") | inherits(i_data, "try-error")) {
    message(paste("Overgeslagen: Fout bij downloaden data voor", current_bank))
    next
  }
  
  # 2. Bereken log-returns
  b_rets <- diff(log(Ad(na.locf(b_data))))
  i_rets <- diff(log(Ad(na.locf(i_data))))
  
  returns_combined <- merge(b_rets, i_rets, all = FALSE)
  colnames(returns_combined) <- c("R_bank", "R_market")
  returns_df <- data.frame(Date = as.Date(index(returns_combined)), coredata(returns_combined))
  
  # 3. Filter speeches voor deze specifieke centrale bank
  relevant_speeches <- speeches_cleaned %>% 
    filter(trimws(CentralBank) == trimws(relevant_cb_name))
  
  message(paste("Gevonden speeches:", nrow(relevant_speeches)))
  
  if(nrow(relevant_speeches) == 0) next
  
  # 4. Loop door de speeches voor deze bank
  for(i in 1:nrow(relevant_speeches)) {
    event_date <- relevant_speeches$Date_Clean[i]
    
    if(is.na(event_date)) next
    
    # Estimation Window: [-250, -30]
    est_df <- returns_df %>% 
      filter(Date >= (event_date - 250) & Date <= (event_date - 30))
    
    # Event Window: Pak de eerste 3 beschikbare beursdagen vanaf de speech datum
    ev_df <- returns_df %>% 
      filter(Date >= event_date & Date <= (event_date + 7)) %>%
      arrange(Date) %>%
      slice(1:3) 
    
    # 5. Berekening Market Model & AR
    if(nrow(est_df) >= 100 & nrow(ev_df) == 3) {
      
      model <- lm(R_bank ~ R_market, data = est_df)
      
      # Voorspel rendement en bereken Abnormal Returns
      ev_df$AR <- ev_df$R_bank - predict(model, newdata = ev_df)
      
      # Sla resultaat op
      results_list[[counter]] <- data.frame(
        Bank         = current_bank,
        CentralBank  = relevant_cb_name,
        Date         = event_date,
        CAR          = sum(ev_df$AR, na.rm = TRUE),
        Tightness    = relevant_speeches$Tightness[i],
        Regulation   = relevant_speeches$Regulation[i],
        Supervision  = relevant_speeches$Supervision[i]
      )
      counter <- counter + 1
    }
  }
}

# --- STAP 3: RESULTAAT SAMENVOEGEN ---
if(length(results_list) > 0) {
  final_event_study_results <- bind_rows(results_list)
  message("SUCCES: De dataset 'final_event_study_results' is aangemaakt!")
  print(head(final_event_study_results))
} else {
  message("FOUT: Controleer of de namen in Bank_Mapping$CentralBank exact matchen met speeches_cleaned.")
}

# 1. Benodigde pakketten laden
if (!require("fixest")) install.packages("fixest")
library(fixest)
library(broom)

# 2. De dataset voorbereiden
# We zorgen dat Bank en CentralBank als 'factors' worden gezien voor de Fixed Effects
analysis_data <- final_event_study_results %>%
  mutate(
    Bank = as.factor(Bank),
    CentralBank = as.factor(CentralBank),
    Year = as.factor(format(Date, "%Y"))
  )

# ------------------------------------------------------------------------------
# MODEL 1: Basis OLS (zonder Fixed Effects)
# ------------------------------------------------------------------------------
model_ols <- feols(CAR ~ Tightness + Regulation + Supervision, 
                   data = analysis_data)

# ------------------------------------------------------------------------------
# MODEL 2: Bank Fixed Effects
# Hiermee controleer je voor alle tijd-onveranderlijke bankkarakteristieken
# ------------------------------------------------------------------------------
model_bank_fe <- feols(CAR ~ Tightness + Regulation + Supervision | Bank, 
                       data = analysis_data)

# ------------------------------------------------------------------------------
# MODEL 3: Bank & Jaar Fixed Effects
# Dit is het meest robuuste model: het controleert voor banken én globale tijds-trends
# ------------------------------------------------------------------------------
model_full_fe <- feols(CAR ~ Tightness + Regulation + Supervision | Bank + Year, 
                       data = analysis_data)

# ------------------------------------------------------------------------------
# RESULTATEN VERGELIJKEN
# ------------------------------------------------------------------------------

# We gebruiken 'etable' om een mooie academische tabel te maken
# De standaardfouten worden automatisch geclusterd op 'Bank' niveau (zeer belangrijk!)
summary_table <- etable(model_ols, model_bank_fe, model_full_fe, 
                        cluster = ~Bank,
                        headers = c("Basis OLS", "Bank FE", "Bank & Year FE"))

print(summary_table)

# ------------------------------------------------------------------------------
# VISUALISATIE: COEFFICIENT PLOT
# ------------------------------------------------------------------------------
# Hiermee zie je in één oogopslag welke variabelen significant zijn
coefplot(model_full_fe, 
         main = "Impact van Speech Inhoud op Bank Returns (CAR [-1, +1])",
         dict = c(Tightness = "Tightness (Hawkishness)", 
                  Regulation = "Regulation Focus", 
                  Supervision = "Supervision Focus"))

