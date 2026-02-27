# --- STAP 0: DATUM & KOLOMNAAM REPARATIE ---
# We zorgen dat de kolom altijd 'Date_Clean' heet om verwarring te voorkomen
speeches_subset <- speeches_subset %>%
  rename_with(~ "Date_Original", matches("^Date$|^date$")) %>%
  mutate(Date_Clean = as.Date(as.character(Date_Original)))

# Controleer of dit gewerkt heeft
print("Check datums in speeches dataset:")
print(head(speeches_subset$Date_Clean))
#rm(b_data)
#rm(b_rets)

# --- STAP 1: INITIALISATIE ---
results_list <- list()
counter <- 1

colnames(Bank_Mapping)

# --- STAP 2: DE SYSTEMATISCHE LOOP ---
for(j in 1:nrow(Bank_Mapping)) {
  
  current_bank       <- Bank_Mapping$Ticker[j]
  current_index      <- Bank_Mapping$Index_Ticker[j]
  relevant_cb_name   <- Bank_Mapping$CentralBank[j]
  relevant_cb_year   <- Bank_Mapping$year[j]
  
  message(paste0("--- ANALYSE START: ", current_bank, " (", relevant_cb_name, ") - FROM YEAR: ",relevant_cb_year,"--"))
  
  # 1. Haal beursdata op
  b_data <- try(getSymbols(current_bank, src = "yahoo", from = paste0(relevant_cb_year-1, "-01-01"),
                           to = paste0(relevant_cb_year, "-12-31"), auto.assign = FALSE), silent = TRUE)
  i_data <- try(getSymbols(current_index, src = "yahoo", from = paste0(relevant_cb_year-1, "-01-01"),
                           to = paste0(relevant_cb_year, "-12-31"), auto.assign = FALSE), silent = TRUE)
  
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
  relevant_speeches <- speeches_subset %>% 
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

#1 malige uitvoering als test
rm(market_returns)
all_tickers <- unique(c(tickers, Bank_Mapping$Index_Ticker))

market_data_list <- lapply(all_tickers, function(x) {
  message(paste("Bezig met downloaden van:", x))
  getSymbols(x, src="yahoo", from="1986-01-01", to="2023-11-30", auto.assign=FALSE)
})
names(market_data_list) <- all_tickers

market_prices <- do.call(merge, lapply(market_data_list, Ad))
colnames(market_prices) <- all_tickers
market_prices_clean <- na.locf(market_prices, na.rm = FALSE)
market_returns <- diff(log(market_prices_clean))
view (market_returns)

#event study
# --- STAP 1: INITIALISATIE ---
results_list <- list()
counter <- 1

# (OPTIONEEL) 1x trimmen voor snelheid en betere matches
Bank_Mapping$CentralBank  <- trimws(Bank_Mapping$CentralBank)
Bank_Mapping$Ticker       <- trimws(Bank_Mapping$Ticker)
Bank_Mapping$Index_Ticker <- trimws(Bank_Mapping$Index_Ticker)

speeches_subset$CentralBank <- trimws(speeches_subset$CentralBank)
speeches_subset$Date_Clean  <- as.Date(speeches_subset$Date_Clean)

# --- STAP 2: DE SYSTEMATISCHE LOOP ---
for(j in 1:nrow(Bank_Mapping)) {
  
  current_bank       <- Bank_Mapping$Ticker[j]
  current_index      <- Bank_Mapping$Index_Ticker[j]
  relevant_cb_name   <- Bank_Mapping$CentralBank[j]
  relevant_cb_year   <- Bank_Mapping$year[j]
  
  message(paste0("--- ANALYSE START: ", current_bank, " (", relevant_cb_name, ") - FROM YEAR: ", relevant_cb_year, "--"))
  
  # 1) ADAPTATION: haal data uit je reeds geladen objecten i.p.v. Yahoo
  # Buffer is belangrijk voor estimation window [-250,-30]
  from_date <- as.Date(paste0(relevant_cb_year - 1, "-01-01"))
  to_date   <- as.Date(paste0(relevant_cb_year,     "-12-31"))
  
  # controle: bestaan tickers in je cache?
  if (!current_bank %in% names(market_returns)) {
    message("Overgeslagen: bank ticker niet in market return: ", current_bank)
    next
  }
  if (!current_index %in% names(market_returns)) {
    message("Overgeslagen: index ticker niet in market return: ", current_index)
    next
  }
  
  # subset op datumbereik
  b_data <- prices_list[[current_bank]][paste0(from_date, "/", to_date)]
  i_data <- prices_list[[current_index]][paste0(from_date, "/", to_date)]
  
  # check: genoeg data?
  if (NROW(b_data) == 0 || NROW(i_data) == 0) {
    message("Overgeslagen: geen data in range voor ", current_bank, " of index ", current_index)
    next
  }
  
  # 2. Bereken log-returns
  b_rets <- diff(log(Ad(na.locf(b_data))))
  i_rets <- diff(log(Ad(na.locf(i_data))))
  
  returns_combined <- merge(b_rets, i_rets, all = FALSE)
  colnames(returns_combined) <- c("R_bank", "R_market")
  returns_df <- data.frame(Date = as.Date(index(returns_combined)), coredata(returns_combined))
  
  # 3. Filter speeches voor deze specifieke centrale bank + (BELANGRIJK) dit jaar
  relevant_speeches <- speeches_subset %>%
    dplyr::filter(CentralBank == relevant_cb_name,
                  lubridate::year(Date_Clean) == relevant_cb_year)
  
  message(paste("Gevonden speeches:", nrow(relevant_speeches)))
  if(nrow(relevant_speeches) == 0) next
  
  # 4. Loop door de speeches voor deze bank
  for(i in 1:nrow(relevant_speeches)) {
    event_date <- relevant_speeches$Date_Clean[i]
    if(is.na(event_date)) next
    
    # Estimation Window: [-250, -30]
    est_df <- returns_df %>%
      dplyr::filter(Date >= (event_date - 250) & Date <= (event_date - 30))
    
    # Event Window: eerste 3 beursdagen vanaf speech datum (zoals jij had)
    ev_df <- returns_df %>%
      dplyr::filter(Date >= event_date & Date <= (event_date + 7)) %>%
      dplyr::arrange(Date) %>%
      dplyr::slice(1:3)
    
    # 5. Market Model & AR
    if(nrow(est_df) >= 100 & nrow(ev_df) == 3) {
      
      model <- lm(R_bank ~ R_market, data = est_df)
      
      ev_df$AR <- ev_df$R_bank - predict(model, newdata = ev_df)
      
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
  final_event_study_results <- dplyr::bind_rows(results_list)
  message("SUCCES: De dataset 'final_event_study_results' is aangemaakt!")
  print(head(final_event_study_results))
} else {
  message("FOUT: Controleer of de namen in Bank_Mapping$CentralBank exact matchen met speeches_cleaned.")
}
