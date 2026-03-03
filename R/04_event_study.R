
colnames(Bank_Mapping)
# --- STAP 2 (start loop): DE ESM LOOP MET EXTRA FOUTCONTROLE ---
results_list <- list()
counter <- 1

for(j in 1:nrow(Bank_Mapping)) {
  
  curr_bank   <- trimws(Bank_Mapping$Ticker[j])
  curr_index  <- trimws(Bank_Mapping$Index_Ticker[j])
  curr_cb     <- trimws(Bank_Mapping$CentralBank[j])
  curr_year   <- Bank_Mapping$year[j]
  
  if (is.null(market_data_list[[curr_bank]]) || is.null(market_data_list[[curr_index]])) next
  
  # 1. Bereken Returns en verwijder NA's direct (CRUCIAAL voor de lm fout)
  b_rets <- diff(log(Ad(na.locf(market_data_list[[curr_bank]]))))
  i_rets <- diff(log(Ad(na.locf(market_data_list[[curr_index]]))))
  
  returns_all <- merge(b_rets, i_rets, all = FALSE)
  colnames(returns_all) <- c("R_bank", "R_market")
  
  # Verwijder rijen waar data ontbreekt om "NA in y" te voorkomen
  returns_df <- data.frame(Date = as.Date(index(returns_all)), coredata(returns_all)) %>%
    filter(!is.na(R_bank) & !is.na(R_market))
  
  # 2. Filter speeches
  relevant_speeches <- speeches_subset %>%
    filter(CentralBank == curr_cb, lubridate::year(Date_Clean) == curr_year)
  
  if(nrow(relevant_speeches) == 0) next
  
  for(i in 1:nrow(relevant_speeches)) {
    t0 <- relevant_speeches$Date_Clean[i]
    
    # Zoek de index van t0 in de handelsdagen
    t0_idx <- which(returns_df$Date >= t0)[1]
    
    # Controleer of er genoeg historie én toekomst is voor de windows
    if(is.na(t0_idx) || t0_idx <= 251 || t0_idx >= (nrow(returns_df) - 1)) next
    
    # --- WINDOWS DEFINIËREN ---
    # Estimation Window: relatief t.o.v. t0_idx
    est_df <- returns_df[(t0_idx + EST_WINDOW_START):(t0_idx + EST_WINDOW_EINDE), ]
    
    # Event Window: relatief t.o.v. t0_idx
    ev_df <- returns_df[(t0_idx + EVENT_WINDOW_START):(t0_idx + EVENT_WINDOW_EINDE), ]
    
    # --- BEREKENING ---
    # Extra check op oneindige of ontbrekende waarden in de window
    if(nrow(est_df) >= 150 && all(is.finite(est_df$R_bank))) {
      
      # Probeer het model te fitten, vang fouten op
      fit <- try(lm(R_bank ~ R_market, data = est_df), silent = TRUE)
      
      if(!inherits(fit, "try-error")) {
        # Bereken Abnormal Returns
        ev_df$AR <- ev_df$R_bank - predict(fit, newdata = ev_df)
        
        results_list[[counter]] <- data.frame(
          Ticker          = curr_bank,
          CentralBank     = curr_cb,
          SpeechDate      = t0,
          CAR             = sum(ev_df$AR, na.rm = TRUE),
          Tightness       = relevant_speeches$Tightness[i],
          Regulation      = relevant_speeches$Regulation[i],
          Supervision     = relevant_speeches$Supervision[i],
          ROA             = Bank_Mapping$`ROA (%)`[j],
          TotalAssets     = Bank_Mapping$`total assets`[j],
          TotalEquity     = Bank_Mapping$`total equity`[j],
          CapProxy        = Bank_Mapping$`Capitalization proxy`[j],
          InterbankRatio  = Bank_Mapping$`Interbank ratio`[j]
        )
        counter <- counter + 1
      }
    }
  }
}

final_esm_data <- bind_rows(results_list)
View(final_esm_data)

