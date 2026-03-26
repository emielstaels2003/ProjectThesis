final_esm_data <- final_esm_data %>%
  mutate(
    # Variabelen
    ROA = as.numeric(as.character(ROA)),
    TotalAssets = as.numeric(as.character(TotalAssets)),
    TotalEquity = as.numeric(as.character(TotalEquity)),
    CapProxy = as.numeric(as.character(CapProxy)),
    InterbankRatio = as.numeric(as.character(InterbankRatio)),
    # Categorieën (factoren laten)
    # Regulation = as.factor(Regulation),
    # Supervision = as.factor(Supervision)
    Regulation = as.numeric(as.character(Regulation)),
    Supervision = as.numeric(as.character(Supervision))
  )

# We gebruiken 'feols' omdat dit de standaard is voor snelle, robuuste regressies
# Model 1: De invloed van tekstintensiteit en toon op CAR
model_baseline <- feols(CAR ~ Regulation + Supervision + Tightness, 
                        data = final_esm_data)
summary(model_baseline)

# Model 2: OLS met bank controls en de lokale crisis indicator
model_2_crisis <- feols(CAR ~ Regulation + Supervision + Tightness + 
                          ROA + log(TotalAssets) + CapProxy + is_GSIB +
                         VIX_Level + crisis, # Jouw lokale crisis variabele
                        data = final_esm_data)

summary(model_2_crisis)

# Model 3: Fixed Effects toevoegen voor Ticker (Bank) en SpeechDate (Tijd)
model_3_fe_robust <- feols(CAR ~ Regulation + Supervision + Tightness + 
                             ROA + log(TotalAssets) + CapProxy | 
                             Ticker + SpeechDate, 
                           cluster = ~Ticker + SpeechDate, # Dubbele clustering
                           data = final_esm_data)

summary(model_3_fe_robust)

# Model 4: Interactie tussen Onderwerp (Regulation/Supervision) en Toon (Tightness)
model_4_interaction <- feols(CAR ~ Regulation * Tightness + 
                               Supervision * Tightness + 
                               ROA + log(TotalAssets) + CapProxy | 
                               Ticker + SpeechDate, 
                             cluster = ~Ticker + SpeechDate, 
                             data = final_esm_data)

summary(model_4_interaction)

# Model 5: De Triple Interaction voor zowel Regulation als Supervision
model_5_final <- feols(CAR ~ Regulation * Tightness * Has_Supervisory_Power + 
                         Supervision * Tightness * Has_Supervisory_Power + 
                         ROA + log(TotalAssets) + CapProxy | 
                         Ticker + SpeechDate, 
                       cluster = ~Ticker + SpeechDate, 
                       data = final_esm_data)

summary(model_5_final)


#enorm veel observaies worden verwijderd, de InterbankRatio is de oorzaak hiervan dus we halen die variabele eruit


#ONDERSTAANDE CODE HANDIG VOOR HET GENEREREN VAN MOOIE OVERZICHTELIJKE TABELLEN
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








