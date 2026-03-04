# --- STAP 2: DE ESM LOOP MET LAGGED MARKET TERM ---
results_list_lagged <- list()
counter <- 1

for(j in 1:nrow(Bank_Mapping)) {
  
  curr_bank   <- trimws(Bank_Mapping$Ticker[j])
  curr_index  <- trimws(Bank_Mapping$Index_Ticker[j])
  curr_cb     <- trimws(Bank_Mapping$CentralBank[j])
  curr_year   <- Bank_Mapping$year[j]
  
  if (is.null(market_data_list[[curr_bank]]) || is.null(market_data_list[[curr_index]])) next
  
  # 1. Bereken Returns
  b_rets <- diff(log(Ad(na.locf(market_data_list[[curr_bank]]))))
  i_rets <- diff(log(Ad(na.locf(market_data_list[[curr_index]]))))
  
  returns_all <- merge(b_rets, i_rets, all = FALSE)
  colnames(returns_all) <- c("R_bank", "R_market")
  
  # --- CRUCIAAL VOOR MODEL 2: LAGGED TERM AANMAKEN ---
  # We maken R_market_lag aan (R_m,t-1)
  returns_df <- data.frame(Date = as.Date(index(returns_all)), coredata(returns_all)) %>%
    mutate(R_market_lag = lag(R_market)) %>%
    filter(!is.na(R_bank) & !is.na(R_market) & !is.na(R_market_lag)) # Verwijder eerste rij met NA 
  
  # 2. Filter speeches
  relevant_speeches <- speeches_subset %>%
    filter(CentralBank == curr_cb, lubridate::year(Date_Clean) == curr_year)
  
  if(nrow(relevant_speeches) == 0) next
  
  for(i in 1:nrow(relevant_speeches)) {
    t0 <- relevant_speeches$Date_Clean[i]
    t0_idx <- which(returns_df$Date >= t0)[1]
    
    if(is.na(t0_idx) || t0_idx <= 251 || t0_idx >= (nrow(returns_df) - 1)) next
    
    # Windows definiëren
    est_df <- returns_df[(t0_idx + EST_WINDOW_START):(t0_idx + EST_WINDOW_EINDE), ]
    ev_df  <- returns_df[(t0_idx + EVENT_WINDOW_START):(t0_idx + EVENT_WINDOW_EINDE), ]
    
    # --- BEREKENING MODEL 2: R_i,t = alpha + beta0*R_m,t + beta1*R_m,t-1 ---
    if(nrow(est_df) >= 150 && all(is.finite(est_df$R_bank))) {
      
      # Regressie met de extra lagged term
      fit_lag <- try(lm(R_bank ~ R_market + R_market_lag, data = est_df), silent = TRUE)
      
      if(!inherits(fit_lag, "try-error")) {
        # Voorspel rendement op basis van huidige én vertraagde marktwaarde
        ev_df$AR <- ev_df$R_bank - predict(fit_lag, newdata = ev_df)
        
        results_list_lagged[[counter]] <- data.frame(
          Ticker          = curr_bank,
          CentralBank     = curr_cb,
          Index_Ticker    = curr_index,
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
          # Voeg andere variabelen toe naar behoefte
        )
        counter <- counter + 1
      }
    }
  }
}

final_esm_lagged_data <- bind_rows(results_list_lagged)
