# --- STAP 0: DATUM & KOLOMNAAM REPARATIE ---
# We zorgen dat de kolom altijd 'Date_Clean' heet om verwarring te voorkomen
speeches_subset <- speeches_subset %>%
  rename_with(~ "Date_Original", matches("^Date$|^date$")) %>%
  mutate(Date_Clean = as.Date(as.character(Date_Original)))

# Controleer of dit gewerkt heeft
print("Check datums in speeches dataset:")
print(head(speeches_subset$Date_Clean))



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




#event study