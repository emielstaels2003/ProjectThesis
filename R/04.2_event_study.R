#FF4
#FF package takes data from US stocks per trading day
install.packages("tidyfinance")
install.packages("xts")
library(tidyfinance)
library(xts)
library(dplyr)
rm(ff3)
rm(final_esm_data_FF3)
rm(ff3_xts)

ff3 <- download_data("factors_ff_3_daily")



ff3 <- ff3 %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("1994-01-01"),
         date <= as.Date("2023-12-31"))
colnames(ff3) <- c("date","mkt_rf", "smb", "hml", "rf")
ff3 <- ff3 %>%
  mutate(across(c(mkt_rf, smb, hml, rf), ~ ./100))


ff3_xts <- xts(
  ff3[,c("mkt_rf","smb","hml","rf")],
  order.by = ff3$date
)

colnames(ff3_xts) <- c("MKT_RF","SMB","HML","RF")

head(ff3)


final_esm_data_FF3 <- final_esm_data %>%
  left_join(ff3, by = c("SpeechDate" = "date"))

#FF model toepassen

results_list <- list()
counter <- 1

for(j in 1:nrow(Bank_Mapping)) {
  
  curr_bank   <- trimws(Bank_Mapping$Ticker[j])
  curr_index  <- trimws(Bank_Mapping$Index_Ticker[j])
  curr_cb     <- trimws(Bank_Mapping$CentralBank[j])
  curr_year   <- Bank_Mapping$year[j]
  
  if (is.null(market_data_list[[curr_bank]]) || is.null(market_data_list[[curr_index]])) next
  
  # Bank returns
  b_rets <- diff(log(Ad(na.locf(market_data_list[[curr_bank]]))))
  
  # Merge with FF3 factors
  returns_all <- merge(b_rets, ff3_xts, all = FALSE)
  colnames(returns_all)[1] <- "R_bank"
  
  returns_df <- data.frame(Date = as.Date(index(returns_all)), coredata(returns_all)) %>%
    filter(!is.na(R_bank),
           !is.na(MKT_RF),
           !is.na(SMB),
           !is.na(HML),
           !is.na(RF)) %>%
    mutate(R_bank_excess = R_bank - RF)
  
  # Filter speeches
  relevant_speeches <- speeches_subset %>%
    filter(CentralBank == curr_cb, lubridate::year(Date_Clean) == curr_year)
  
  if(nrow(relevant_speeches) == 0) next
  
  for(i in 1:nrow(relevant_speeches)) {
    
    t0 <- as.Date(relevant_speeches$Date_Clean[i])
    
    t0_idx <- which(returns_df$Date >= t0)[1]
    if(is.na(t0_idx) || t0_idx <= 251 || t0_idx >= (nrow(returns_df) - 1)) next
    
    est_df <- returns_df[(t0_idx + EST_WINDOW_START):(t0_idx + EST_WINDOW_EINDE), ]
    ev_df  <- returns_df[(t0_idx + EVENT_WINDOW_START):(t0_idx + EVENT_WINDOW_EINDE), ]
    
    if(nrow(est_df) >= 150 && all(is.finite(est_df$R_bank_excess))) {
      
      fit <- try(lm(R_bank_excess ~ MKT_RF + SMB + HML, data = est_df), silent = TRUE)
      
      if(!inherits(fit, "try-error")) {
        
        ev_df$AR <- ev_df$R_bank_excess - predict(fit, newdata = ev_df)
        
        results_list[[counter]] <- data.frame(
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
        )
        
        counter <- counter + 1
        print(counter)
      }
    }
  }
}

final_esm_data_FF3 <- bind_rows(results_list)
view(final_esm_data_FF3)
