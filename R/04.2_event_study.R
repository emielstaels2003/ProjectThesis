#FF4
#FF package takes data from US stocks per trading day
install.packages("tidyfinance")
install.packages("xts")
library(tidyfinance)
library(xts)
library(dplyr)
library(quantmod)
#rm(ff3)
#rm(final_esm_data_FF3)
#rm(ff3_xts)

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

#FF model toepassen

results_list_FF <- list()
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
        
        results_list_FF[[counter]] <- data.frame(
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

final_esm_data_FF3 <- bind_rows(results_list_FF)
#View(final_esm_data_FF3)

final_esm_data_FF3 <- final_esm_data_FF3 %>%
  mutate(SpeechDate = as.Date(SpeechDate),
         Index_Ticker = trimws(Index_Ticker)) %>%
  mutate(crisis = 0L)

# --- 1) Wereldwijde crises (apply to all banks within interval)
cr_world <- Crisis %>%
  rename(start_date = `start date`, end_date = `end date`) %>%
  mutate(start_date = as.Date(start_date),
         end_date   = as.Date(end_date),
         worldwide  = toupper(trimws(worldwide))) %>%
  filter(worldwide == "YES") %>%
  select(start_date, end_date)

final_esm_data_FF3 <- final_esm_data_FF3 %>%
  mutate(crisis = ifelse(
    crisis == 1L |
      rowSums(sapply(1:nrow(cr_world), function(i)
        SpeechDate >= cr_world$start_date[i] & SpeechDate <= cr_world$end_date[i]
      )) > 0,
    1L, crisis
  ))

library(tidyr)

# --- 2) Lokale crises (apply only if Index_Ticker matches AND within interval)
cr_local <- Crisis %>%
  rename(start_date = `start date`, end_date = `end date`) %>%
  mutate(start_date = as.Date(start_date),
         end_date   = as.Date(end_date),
         worldwide  = toupper(trimws(worldwide))) %>%
  filter(worldwide != "YES") %>%
  pivot_longer(cols = c(index, index2), values_to = "Index_Ticker") %>%
  mutate(Index_Ticker = trimws(Index_Ticker)) %>%
  filter(!is.na(Index_Ticker), Index_Ticker != "") %>%
  select(start_date, end_date, Index_Ticker)

# Wijziging: controleer de conditie per rij met rowwise en any() om dubbele rijen te voorkomen
final_esm_data_FF3 <- final_esm_data_FF3 %>%
  rowwise() %>%
  mutate(crisis = ifelse(
    crisis == 1L | any(
      cr_local$Index_Ticker == Index_Ticker & 
        SpeechDate >= cr_local$start_date & 
        SpeechDate <= cr_local$end_date
    ),
    1L, 
    crisis
  )) %>%
  ungroup()

# --- Vervolg: Supervision dates, VIX, G-SIB, abs_CAR

final_esm_data_FF3 <- final_esm_data_FF3 %>%
  # 1. Plak de startdata uit de referentietabel aan je dataset
  left_join(supervision_dates, by = c("CentralBank" = "bank")) %>%
  
  # 2. Maak de dummy aan
  mutate(Has_Supervisory_Power = ifelse(
    !is.na(direct_supervisory_power_date) & SpeechDate >= direct_supervisory_power_date, 
    1, 
    0
  )) %>%
  
  # 3. Optioneel: verwijder de hulp-datumkolom weer als je die niet nodig hebt
  select(-direct_supervisory_power_date)

# Check het resultaat
head(final_esm_data_FF3)

#View(final_esm_data_FF3)

final_esm_data_FF3$SpeechDate <- as.Date(final_esm_data_FF3$SpeechDate)
# Voeg de VIX kolom toe aan de dataset
final_esm_data_FF3 <- final_esm_data_FF3 %>%
  left_join(vix_df, by = "SpeechDate") %>%
  arrange(SpeechDate) %>%
  # Vul ontbrekende waarden (weekenden) in met de laatst bekende koers
  fill(VIX_Level, .direction = "down") 


# 2. Voeg de kolom toe aan final_esm_data_FF3
final_esm_data_FF3 <- final_esm_data_FF3 %>%
  mutate(is_GSIB = ifelse(Ticker %in% gsib_tickers, 1, 0))

# 3. Controle: Hoeveel observaties zijn gelinkt aan een G-SIB?
table(final_esm_data_FF3$is_GSIB)


# Maak een nieuwe kolom aan met de absolute waarde van de CAR
final_esm_data_FF3$abs_CAR <- abs(final_esm_data_FF3$CAR)


# Eigen robuuste winsorize functie
simple_winsorize <- function(x) {
  # Bereken de grenzen (1% en 99%)
  q <- quantile(x, probs = c(0.01, 0.99), na.rm = TRUE)
  # Stel waarden buiten de grenzen gelijk aan de grenzen
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  return(x)
}

# Pas toe op je dataset
message("Bezig met handmatige winsorizing van de data...")

final_esm_data_FF3 <- final_esm_data_FF3 %>%
  mutate(
    CAR         = simple_winsorize(as.numeric(CAR)),
    abs_CAR     = simple_winsorize(as.numeric(abs_CAR)),
    ROA         = simple_winsorize(as.numeric(ROA)),
    CapProxy    = simple_winsorize(as.numeric(CapProxy)),
    TotalAssets = simple_winsorize(as.numeric(TotalAssets))
  )

