#dir.create(PATH_PROCESSED, showWarnings = FALSE, recursive = TRUE)

#saveRDS(speeches_clean, file.path(PATH_PROCESSED, "speeches_clean.rds"))
#saveRDS(prices_clean,   file.path(PATH_PROCESSED, "prices_clean.rds"))


speeches <- readRDS("~/ProjectThesis/data/CBS_dataset_v1.0.rds")

cb_counts <- table(speeches$CentralBank)
cb_matrix <- cbind(CentralBank = names(cb_counts),
                   Observations = as.numeric(cb_counts))
cb_matrix


# DE DATASET SPEECHES KLEINER MAKEN DOOR ENKEL DE G20 TE SELECTEREN 


target_banks <- c(
  "European Central Bank",
  "Board of Governors of the Federal Reserve", 
  "Reserve Bank of Australia",
  "Bank of Mexico",
  "Bank of Canada",
  "Bank of Japan",
  "Bank of England",
  "Central Bank of Argentina",
  "Central Bank of Brazil",
  "People's Bank of China",
  "Reserve Bank of India",
  "Bank Indonesia",
  "Bank of Korea",
  "Bank of Russia",
  "Saoedi Central Bank",
  "Central Bank of the Republic of Turkey",
  "South African Reserve Bank",
  "Swiss National Bank "
)

tickers <- c(
  "BNP.PA", "GLE.PA", "ACA.PA", "DBK.DE", "CBK.DE", "SAN.MC", "BBVA.MC", 
  "UCG.MI", "INGA.AS", "NDA-FI.HE", "JPM", "BAC", "C", "GS", "MS", "WFC", 
  "BK", "STT", "NTRS", "COF", "PNC", "TFC", "USB", "SCHW", "ALLY", "AXP", 
  "CFG", "FITB", "FCNCA", "HBAN", "KEY", "MTB", "RF", "SYF", 
  "ANZ.AX", "CBA.AX", "NAB.AX", "WBC.AX", "RY.TO", "TD.TO", "BMO.TO", 
  "BNS.TO", "CM.TO", "NA.TO", "8306.T", "8316.T", "8411.T", "8604.T", 
  "HSBA.L", "BARC.L", "STAN.L", "NWG.L", "LLOY.L", "GGAL.BA", "BMA.BA", 
  "BPAT.BA", "BBAR.BA", "ITUB4.SA", "BBDC4.SA", "BBAS3.SA", "SANB11.SA", 
  "601398.SS", "601988.SS", "601288.SS", "601939.SS", "601328.SS", 
  "600036.SS", "601166.SS", "601998.SS", "600000.SS", "601658.SS", 
  "SBIN.NS", "HDFCBANK.NS", "ICICIBANK.NS", "BMRI.JK", "BBRI.JK", 
  "BBCA.JK", "BBNI.JK", "BBTN.JK", "024110.KS", "SBER.ME", "VTBR.ME", 
  "CBOM.ME", "1180.SR", "1120.SR", "1010.SR", 
  "1060.SR", "1080.SR", "SBK.JO", "ABG.JO", "NED.JO", "CPI.JO", "FSR.JO", 
  "GFNORTEO.MX", "GFINBURO.MX", "AKBNK.IS", "GARAN.IS", 
  "ISCTR.IS", "HALKB.IS", "VAKBN.IS", "UBSG.SW"
)
aantal <- length(tickers)
# Toon het resultaat in de console
print(paste("De lijst bevat", aantal, "tickers."))

market_data_list <- lapply(tickers, function(x) {
  message(paste("Bezig met downloaden van:", x))
  getSymbols(x, src = "yahoo", from = "1986-01-01", to = "2023-11-30", auto.assign = FALSE)
})



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

# 5. Bekijk het resultaat
View(speeches_subset)



Crisis <- read_excel("data/Crisis.xlsx")
View(Crisis)



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

getSymbols("KSA", from = "2000-01-01", to = Sys.Date())
chartSeries(KSA,
            type = "line",             # lijnplot
            subset = "2000::2025",     # periode
            theme = chartTheme("white"), 
            name = "KSA (Adjusted Close Price)",
            TA = NULL)                 # Geen extra indicatoren






