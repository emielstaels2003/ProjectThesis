final_esm_data <- final_esm_data %>%
  mutate(
    ROA = as.numeric(as.character(ROA)),
    TotalAssets = as.numeric(as.character(TotalAssets)),
    Regulation = as.factor(Regulation),   # Dit moet wel een categorie blijven
    Supervision = as.factor(Supervision)  # Dit ook
  )

# We voegen 'Ticker' toe aan de fixed effects (na de '|')
reg_final <- feols(CAR ~ i(Regulation) + i(Supervision) + Tightness + 
                     i(Regulation):Tightness + ROA + log(TotalAssets) | 
                     Ticker + lubridate::year(SpeechDate), 
                   cluster = ~CentralBank,
                   data = final_esm_data)

summary(reg_final)

# Filter exact op de naam in jouw dataset
us_data <- final_esm_data %>% 
  filter(CentralBank == "Board of Governors of the Federal Reserve")
print(paste("Aantal observaties voor de Fed:", nrow(us_data)))

reg_us <- feols(CAR ~ i(Regulation) + i(Supervision) + Tightness + 
                   i(Regulation):Tightness + ROA + log(TotalAssets) | 
                   Ticker + lubridate::year(SpeechDate), 
                 cluster = ~Ticker, 
                 data = us_data)

# 3. Toon de resultaten
summary(reg_us)

sort(unique(final_esm_data$CentralBank))

# 4. BONUS: Zet de Wereld-resultaten en de VS-resultaten naast elkaar
# Zo zie je direct het verschil in de sterretjes
etable(reg_final, reg_usa, 
       headers = c("Wereldwijd", "Verenigde Staten"),
       tex = FALSE)


# 1. Tel het aantal observaties met de exacte naam (gekopieerd van jouw bericht)
exact_fed_count <- sum(final_esm_data$CentralBank == "Board of Governors of the Federal Reserve", na.rm = TRUE)

# 2. Zoek naar namen die "Federal Reserve" bevatten (voor het geval er spaties omheen staan)
fuzzy_fed_count <- sum(grepl("Federal Reserve", final_esm_data$CentralBank, ignore.case = TRUE))

# 3. Toon de resultaten
print(paste("Aantal met de exacte naam:", exact_fed_count))
print(paste("Aantal met 'fuzzy' zoekopdracht:", fuzzy_fed_count))

# 4. Als er een verschil is, laat dit dan de exacte spelling zien die R ziet
if (fuzzy_fed_count > exact_fed_count) {
  message("Let op: er zijn meer matches gevonden met de zoekopdracht. Hier is de spelling in de data:")
  unieke_namen <- unique(final_esm_data$CentralBank[grepl("Federal Reserve", final_esm_data$CentralBank, ignore.case = TRUE)])
  # We zetten er aanhalingstekens omheen om spaties zichtbaar te maken
  print(paste0("'", unieke_namen, "'"))
}




# --- GRAFIEK 1: Verdeling van de CARs ---
# Hiermee zie je of je data 'normaal' verdeeld is en of er uitschieters zijn.
ggplot(final_esm_data, aes(x = CAR)) +
  geom_histogram(bins = 100, fill = "dodgerblue", color = "white", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  coord_cartesian(xlim = c(-0.05, 0.05)) + # We zoomen in op de relevante range
  labs(title = "Verdeling van Cumulative Abnormal Returns (CAR [-1, 1])",
       subtitle = "Rode lijn is het nulpunt (geen effect)",
       x = "CAR (0.01 = 1%)",
       y = "Aantal observaties") +
  theme_minimal()

# --- GRAFIEK 2: CAR per Regulation Groep (Boxplot) ---
# Dit is de visuele check van je significante resultaat.
ggplot(final_esm_data, aes(x = as.factor(Regulation), y = CAR, fill = as.factor(Regulation))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) + 
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  coord_cartesian(ylim = c(-0.01, 0.015)) +
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A"), 
                    labels = c("Geen (0)", "Midden (1)", "Hoog (2)")) +
  labs(title = "Gemiddelde CAR per Regulation Groep",
       subtitle = "Zwarte stip is het gemiddelde (0 = Geen trefwoorden, 2 = Veel trefwoorden)",
       x = "Regulation Intensiteit",
       y = "CAR [-1, 1]",
       fill = "Groep") +
  theme_minimal()

# --- GRAFIEK 3: Coefficient Plot ---
# Een simpele manier om je tabel visueel te maken zonder alle cijfers.
# Hiervoor gebruiken we de library 'broom' (die had je al geladen)
library(broom)
reg_plot_data <- tidy(reg_final) %>% 
  filter(!grepl("ROA|TotalAssets", term)) # We focussen op de hoofdvariabelen

ggplot(reg_plot_data, aes(x = estimate, y = term)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(title = "Effect van variabelen op de CAR (Coëfficiënt Plot)",
       subtitle = "Met 95% betrouwbaarheidsinterval. Als de lijn 0 niet raakt, is het significant.",
       x = "Geschat effect (Estimate)",
       y = "Variabele") +
  theme_minimal()







