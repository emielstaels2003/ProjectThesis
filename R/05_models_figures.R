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

library(fixest)

model0 <- feols(CAR ~ Regulation + Supervision, 
                        data = final_esm_data)
summary(model0)

model11 <- feols(CAR ~ (Supervision + Regulation), data=final_esm_data)
summary(model11)

# We gebruiken 'feols' omdat dit de standaard is voor snelle, robuuste regressies
# Model 1: De invloed van tekstintensiteit en toon op CAR
model_baseline <- feols(CAR ~ Regulation + Supervision + Tightness, 
                        data = final_esm_data)
summary(model_baseline)

# Model 2: OLS met bank controls en de lokale crisis indicator
model_2_crisis <- feols(CAR ~ Regulation + Supervision + Tightness + 
                          ROA + log(TotalAssets) + CapProxy + is_GSIB +
                         VIX_Level + crisis,
                        data = final_esm_data)

summary(model_2_crisis)

# Model 3: Fixed Effects toevoegen voor Ticker (Bank) en SpeechDate (Tijd)
model_3_fe_robust <- feols(CAR ~ Regulation + Supervision + Tightness * VIX_Level + 
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



library(fixest)

# Model 1a: Richting - Bewegen de koersen omhoog of omlaag door de inhoud?
m1a_baseline <- feols(CAR ~ Regulation + Supervision, 
                      data = final_esm_data)
summary(m1a_baseline)
# Model 1b: Intensiteit - Zorgt de inhoud voor een schok (ongeacht positief/negatief)?
m1b_abs_baseline <- feols(abs_CAR ~ Regulation + Supervision, 
                          data = final_esm_data)
summary(m1b_abs_baseline)


# Model 2a: Richting (CAR) - Test van H2
# We kijken of een hogere 'Tightness' zorgt voor een daling in de koers.
m2a_tone <- feols(CAR ~ Regulation + Supervision + Tightness, 
                  data = final_esm_data)
summary(m2a_tone)

# Model 2b: Intensiteit (|CAR|) 
# Zorgt een strengere toon voor grotere schokken in de markt?
m2b_abs_tone <- feols(abs_CAR ~ Regulation + Supervision + Tightness, 
                      data = final_esm_data)
summary(m2b_abs_tone)


# Model 3a: Richting (CAR) met Bank Controls
m3a_controls <- feols(CAR ~ Regulation + Supervision + Tightness + 
                        ROA + log(TotalAssets) + CapProxy, 
                      data = final_esm_data)
summary(m3a_controls)

# Model 3b: Intensiteit (|CAR|) met Bank Controls
m3b_abs_controls <- feols(abs_CAR ~ Regulation + Supervision + Tightness + 
                            ROA + log(TotalAssets) + CapProxy, 
                          data = final_esm_data)
summary(m3b_abs_controls)


# Model 4a: Richting (CAR) met High-Dimensional Fixed Effects
m4a_fe <- feols(CAR ~ Regulation + Supervision + Tightness + 
                  ROA + log(TotalAssets) + CapProxy | Ticker + SpeechDate, 
                data = final_esm_data)
summary(m4a_fe)

# Model 4b: Intensiteit (|CAR|) met High-Dimensional Fixed Effects
m4b_abs_fe <- feols(abs_CAR ~ Regulation + Supervision + Tightness + 
                      ROA + log(TotalAssets) + CapProxy | Ticker + SpeechDate, 
                    data = final_esm_data)
summary(m4b_abs_fe)




# We maken even een jaar-maand variabele aan
final_esm_data$year_month <- format(as.Date(final_esm_data$SpeechDate), "%Y-%m")

# Model 4c: Richting met Maand-Fixed Effects (minder streng dan dag)
m4c_fe_month <- feols(CAR ~ Regulation + Supervision + Tightness + 
                        ROA + log(TotalAssets) + CapProxy | Ticker + year_month, 
                      data = final_esm_data)

# Bekijk of Tightness nu weer terugkomt
summary(m4c_fe_month)


# Model 5a: De Kern-Interactie (Richting)
# Is praten over regulering pas pijnlijk als de toon streng is?
m5a_core_interact <- feols(CAR ~ Regulation * Tightness + Supervision * Tightness + 
                             ROA + log(TotalAssets) + CapProxy | Ticker + year_month, 
                           data = final_esm_data)
summary(m5a_core_interact)

# Model 5b: De Kern-Interactie (Intensiteit)
# Zorgt de combinatie van 'wat' en 'hoe' voor grotere marktschokken?
m5b_abs_core_interact <- feols(abs_CAR ~ Regulation * Tightness + Supervision * Tightness + 
                                 ROA + log(TotalAssets) + CapProxy | Ticker + year_month, 
                               data = final_esm_data)
summary(m5b_abs_core_interact)





# Model 5a: Richting (CAR) - Wordt de koersdaling door Tightness erger bij macht?
m5a_h3_car <- feols(CAR ~ Regulation + Supervision + Tightness * Has_Supervisory_Power + 
                      ROA + log(TotalAssets) + CapProxy | Ticker + year_month, 
                    data = final_esm_data)
summary(m5a_h3_car)

# Model 5b: Intensiteit (|CAR|) - Zorgt macht voor grotere schokken bij Tightness?
m5b_h3_abs <- feols(abs_CAR ~ Regulation + Supervision + Tightness * Has_Supervisory_Power + 
                      ROA + log(TotalAssets) + CapProxy | Ticker + year_month, 
                    data = final_esm_data)
summary(m5b_h3_abs)


# Stap 5c: De "Power Play"
# Vraag: Is de impact van Tightness afhankelijk van de macht van de centrale bank?
m5c_power_interact <- feols(CAR ~ Regulation + Supervision + Tightness * Has_Supervisory_Power + 
                              ROA + log(TotalAssets) + CapProxy | Ticker + year_month, 
                            data = final_esm_data)
summary(m5c_power_interact)

# Stap 5d: De Intensiteit van Macht
m5d_abs_power_interact <- feols(abs_CAR ~ Regulation + Supervision + Tightness * Has_Supervisory_Power + 
                                  ROA + log(TotalAssets) + CapProxy | Ticker + year_month, 
                                data = final_esm_data)
summary(m5d_abs_power_interact)




# Model 6a: De Crisis Interactie
# Wordt de impact van Tightness versterkt tijdens de financiële crisis?
m6a_crisis <- feols(CAR ~ Regulation + Supervision + Tightness * crisis + 
                      ROA + log(TotalAssets) + CapProxy | Ticker + year_month, 
                    data = final_esm_data)
summary(m6a_crisis)

# Model 6b: De VIX Interactie (Marktonzekerheid)
# Is de markt reactiever op toon als de algemene angst (VIX) hoog is?
m6b_vix <- feols(CAR ~ Regulation + Supervision + Tightness * VIX_Level + 
                   ROA + log(TotalAssets) + CapProxy | Ticker + year_month, 
                 data = final_esm_data)
summary(m6b_vix)

m_final_triple <- feols(CAR ~ Regulation * Tightness * Has_Supervisory_Power + 
                          Supervision * Tightness * Has_Supervisory_Power +
                          VIX_Level + ROA + log(TotalAssets) + CapProxy | Ticker + year_month, 
                        data = final_esm_data)
summary(m_final_triple)

# De versie voor Intensiteit (|CAR|)
m_final_abs_triple <- feols(abs_CAR ~ Regulation * Tightness * Has_Supervisory_Power + 
                              Supervision * Tightness * Has_Supervisory_Power +
                              VIX_Level + ROA + log(TotalAssets) + CapProxy | Ticker + year_month, 
                            data = final_esm_data)
summary(m_final_abs_triple)
