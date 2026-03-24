speeches <- readRDS("~/ProjectThesis/data/CBS_dataset_v1.0.rds")

cb_counts <- table(speeches$CentralBank)
cb_matrix <- cbind(CentralBank = names(cb_counts),
                  Observations = as.numeric(cb_counts))
cb_matrix

# Filter de speeches_cleaned dataset
# De %in% operator kijkt of de waarde in CentralBank voorkomt in onze lijst
speeches_subset <- speeches[speeches$CentralBank %in% target_banks, ]

speeches_subset$Date <- as.Date(speeches_subset$Date)

# 3. Filteren op de juiste kolomnaam 'Date'
speeches_subset <- speeches_subset[
  !is.na(speeches_subset$Date) & 
    speeches_subset$Date >= as.Date("1997-01-01") & 
    speeches_subset$Date <= as.Date("2023-12-31"), 
]

speeches$Date <- as.Date(speeches$Date)

# 2. Filter de originele dataset op European Central Bank
ecb_speeches <- speeches[speeches$CentralBank == "European Central Bank", ]

# 3. Zoek de minimum datum (de oudste speech)
eerste_datum <- min(ecb_speeches$Date, na.rm = TRUE)

# Toon het resultaat
print(eerste_datum)



# 5. Bekijk het resultaat
#View(speeches_subset)


Bank_Mapping <- read_excel("data/Bank_Year_CB_Data.xlsx")
#View(Bank_Mapping)


Crisis <- read_excel("data/CRISISFILE.xlsx")
#View(Crisis)



# --- STAP 0: DATUM & KOLOMNAAM REPARATIE ---
# We zorgen dat de kolom altijd 'Date_Clean' heet om verwarring te voorkomen
speeches_subset <- speeches_subset %>%
  rename_with(~ "Date_Original", matches("^Date$|^date$")) %>%
  mutate(Date_Clean = as.Date(as.character(Date_Original)))


# --- YAHOO: DATA CLEANING & CACHING ---
all_tickers <- unique(c(Bank_Mapping$Ticker, Bank_Mapping$Index_Ticker))

message("Bezig met ophalen van wereldwijde beursdata...")
market_data_list <- lapply(all_tickers, function(x) {
  tryCatch({
    getSymbols(x, src="yahoo", from="1996-01-01", to="2023-12-31", auto.assign=FALSE)
  }, error = function(e) return(NULL))
})
names(market_data_list) <- all_tickers

#VIX koers inlezen
getSymbols("^VIX", src = "yahoo", from = "1997-01-01")
vix_df <- data.frame(
  SpeechDate = index(VIX),
  VIX_Level = as.numeric(Ad(VIX))
)

# TEST VOOR STOCK PRICE DATA: GRAFIEK MAKEN VAN STOCK PRICE

getSymbols("^STOXX50E", from = "1997-01-01", to = Sys.Date())
chartSeries(STOXX50E,
            type = "line",             # lijnplot
            subset = "1997::2025",     # periode
            theme = chartTheme("white"), 
            name = "STOXX50E (Adjusted Close Price)",
            TA = NULL)                 # Geen extra indicatoren

data_framed <- supervision_dates %>%
  left_join(speeches_subset, by = c("bank" = "CentralBank"))
setdiff(supervision_dates$bank, unique(speeches_subset$CentralBank))
data_framed <- speeches_subset %>%
  left_join(supervision_dates, by = c("CentralBank" = "bank"))
dim(data_framed)
colnames(data_framed)
table(is.na(data_framed$direct_supervisory_power_date))
data_framed <- data_framed %>%
  mutate(
    Date_Clean = as.Date(Date_Clean),
    direct_supervisory_power_date = as.Date(direct_supervisory_power_date),
    post_supervision = ifelse(
      !is.na(direct_supervisory_power_date) &
        Date_Clean >= direct_supervisory_power_date,
      1, 0
    )
  )
table(data_framed$post_supervision, useNA = "ifany")
data_framed %>%
  group_by(CentralBank) %>%
  summarise(
    min_post = min(post_supervision),
    max_post = max(post_supervision)
  )
