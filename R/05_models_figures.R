final_esm_data <- final_esm_data %>%
  mutate(
    # Variabelen
    ROA = as.numeric(as.character(ROA)),
    TotalAssets = as.numeric(as.character(TotalAssets)),
    TotalEquity = as.numeric(as.character(TotalEquity)),
    CapProxy = as.numeric(as.character(CapProxy)),
    InterbankRatio = as.numeric(as.character(InterbankRatio)),
    # Categorieën (factoren laten)
    Regulation = as.factor(Regulation),
    Supervision = as.factor(Supervision)
  )


# We voegen 'Ticker' toe aan de fixed effects (na de '|')
reg_final <- feols(CAR ~ i(Regulation) * Sentiment + i(Supervision) * Sentiment + 
                        i(Regulation) * Tightness + i(Supervision) * Tightness +
                        ROA + log(TotalAssets) + TotalEquity + 
                        CapProxy + InterbankRatio | 
                        Ticker + lubridate::year(SpeechDate),
                      cluster = ~CentralBank,
                      data = final_esm_data)
summary(reg_final)
#enorm veel observaies worden verwijderd, de InterbankRatio is de oorzaak hiervan dus we halen die variabele eruit

basismodel <- feols(CAR ~ i(Regulation) + i(Supervision) + Tightness | Ticker + lubridate::year(SpeechDate), cluster = ~CentralBank, data = final_esm_data)
summary(basismodel)

basismodel_sentiment <- feols(CAR ~ i(Regulation) + i(Supervision) + Tightness + Sentiment | Ticker + lubridate::year(SpeechDate), cluster = ~CentralBank, data = final_esm_data)
summary(basismodel_sentiment)

interactie_model <- feols(CAR ~ i(Regulation)*Sentiment + i(Supervision)*Sentiment + i(Regulation)*Tightness + i(Supervision)*Tightness | Ticker + lubridate::year(SpeechDate), cluster = ~CentralBank, data = final_esm_data)
summary(interactie_model)

all_in_model <- feols(CAR ~ i(Regulation) * Sentiment + i(Supervision) * Sentiment + 
                        i(Regulation) * Tightness + i(Supervision) * Tightness +
                        ROA + log(TotalAssets) + TotalEquity + 
                        CapProxy | 
                        Ticker + lubridate::year(SpeechDate),
                      cluster = ~CentralBank,
                      data = final_esm_data)
summary(all_in_model)

library(modelsummary)

# Maak een lijst van je vier modellen
modellen_lijst <- list(
  "(1) Basis"           = basismodel,
  "(2) + Sentiment"      = basismodel_sentiment,
  "(3) Interactie"      = interactie_model,
  "(4) All-in"          = all_in_model
)

# Genereer de tabel
msummary(modellen_lijst,
         stars = TRUE,           # Voegt de sterretjes toe (*** p<0.001, etc.)
         fmt = 4,                # Rondt af op 4 decimalen (vervangt de e-03 notatie)
         estimate = "estimate",  # Toont de coëfficiënt
         statistic = "std.error",# Toont standaardfout tussen haakjes daaronder
         gof_omit = "AIC|BIC|Log|RMSE", # Verbergt overbodige statistieken
         title = "Tabel: Impact of Central Bank Communication on CAR",
         notes = "Standard errors are clustered on central bank level")




reg_final2 <- feols(CAR ~ i(Regulation) * Sentiment + i(Supervision) * Sentiment + 
                      i(Regulation) * Tightness + i(Supervision) * Tightness +
                      ROA + log(TotalAssets) + TotalEquity + 
                      CapProxy | 
                      Ticker + lubridate::year(SpeechDate),
                    cluster = ~CentralBank,
                    data = final_esm_data)
summary(reg_final2)
# OBSERVATIE1
# Regulation2 significant: Wanneer centrale banken intensief communiceren over nieuwe regelgeving (niveau 2),
# reageert de markt consistent positief. Dit suggereert dat beleggers duidelijkheid over regels prefereren boven onzekerheid, 
# wat de "clarity" hypothese uit de literatuur ondersteunt.

# OBSERVATIE2
# CapProxy significant: De negatieve coëfficiënt is heel interessant. 
# Het suggereert dat banken met een hogere kapitaalratio juist een 
# kleinere (minder positieve) koersreactie hebben op regulatie-speeches. 
# Logisch: zij zijn al veilig en hebben minder baat bij nieuwe stabiliteitsregels dan banken die er zwakker voorstaan.

reg_final3 <- feols(CAR ~ i(Regulation) + i(Supervision) + Tightness +
    i(Regulation):Tightness +
    i(Supervision):Tightness +
    crisis +
    ROA + log(TotalAssets) + TotalEquity + CapProxy |
    Ticker + lubridate::year(SpeechDate),
  cluster = ~CentralBank,
  data = final_esm_data
)
summary(reg_final3)



# 3. Zorg dat je SpeechDate in final_esm_data ook echt een 'Date' type is
final_esm_data$SpeechDate <- as.Date(final_esm_data$SpeechDate)
# Voeg de VIX kolom toe aan de dataset
final_esm_data <- final_esm_data %>%
  left_join(vix_df, by = "SpeechDate") %>%
  arrange(SpeechDate) %>%
  # Vul ontbrekende waarden (weekenden) in met de laatst bekende koers
  fill(VIX_Level, .direction = "down") 

# VIX toevoegen
reg_final3 <- feols(CAR ~ i(Regulation) + i(Supervision) + Tightness + 
                      i(Regulation):Tightness +
                      i(Supervision):Tightness +
                      ROA + log(TotalAssets) + TotalEquity + CapProxy + VIX_Level | 
                      Ticker + lubridate::year(SpeechDate),
                    cluster = ~CentralBank,
                    data = final_esm_data)

summary(reg_final3)

# Filter exact op de naam in jouw dataset
us_data <- final_esm_data %>% 
  filter(CentralBank == "Board of Governors of the Federal Reserve")

reg_us <- feols(CAR ~ i(Regulation) + i(Supervision) + Tightness + 
                  i(Regulation):Tightness +
                  i(Supervision):Tightness +
                  ROA + log(TotalAssets) + TotalEquity + CapProxy + VIX_Level | 
                  Ticker + lubridate::year(SpeechDate),
                 cluster = ~Ticker, 
                 data = us_data)
summary(reg_us)
# OBSERVATIE: zowel regulation1 als regulation2 zijn significant maar in tegengestelde richting

eu_data <- final_esm_data %>% 
  filter(CentralBank == "European Central Bank")

reg_eu <- feols(CAR ~ i(Regulation) + i(Supervision) + Tightness + 
                  i(Regulation):Tightness +
                  i(Supervision):Tightness +
                  ROA + log(TotalAssets) + TotalEquity + CapProxy | 
                  Ticker + lubridate::year(SpeechDate),
                cluster = ~Ticker, 
                data = eu_data)
summary(reg_eu)

eng_data <- final_esm_data %>% 
  filter(CentralBank == "Bank of England")
reg_eng <- feols(CAR ~ i(Regulation) + i(Supervision) + Tightness + 
                      i(Regulation):Tightness +
                      i(Supervision):Tightness +
                      ROA + log(TotalAssets) + TotalEquity + CapProxy | 
                      Ticker + lubridate::year(SpeechDate),
                    cluster = ~Ticker,
                    data = eng_data)
summary(reg_eng)


# Gebruik het modelplot pakket (onderdeel van modelsummary)
# Maak een lijst van je modellen
modellen <- list(
  "Federal Reserve (USA)" = reg_us,
  "European Central Bank (EU)" = reg_eu
)

# Plot de coëfficiënten van Regulation en Supervision
modelplot(modellen, coef_omit = "Intercept|ROA|Total|Cap|Interbank") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "red") +
  labs(title = "Vergelijking Impact: Fed vs. ECB",
       subtitle = "Effect van Centrale Bank communicatie op Bank CARs",
       x = "Coëfficiënt (Impact op rendement)",
       y = "Variabele") +
  theme_minimal()


# --- GRAFIEK 1: Verdeling van de CARs ---
# Hiermee zie je of je data 'normaal' verdeeld is en of er uitschieters zijn.
ggplot(final_esm_data, aes(x = CAR)) +
  geom_histogram(bins = 100, fill = "dodgerblue", color = "white", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  coord_cartesian(xlim = c(-0.05, 0.05)) + # We zoomen in op de relevante range
  labs(title = "Distribution of Cumulative Abnormal Returns (CAR [-1, 1])",
       subtitle = "The dashed red line indicates the zero-effect baseline",
       x = "CAR (0.01 = 1%)",
       y = "Frequency (Number of Observations)") +
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

