#model

# 2. De dataset voorbereiden
# We zorgen dat Bank en CentralBank als 'factors' worden gezien voor de Fixed Effects
analysis_data <- final_event_study_results %>%
  mutate(
    Bank = as.factor(Bank),
    CentralBank = as.factor(CentralBank),
    Year = as.factor(format(Date, "%Y"))
  )



# ------------------------------------------------------------------------------
# MODEL 1: Basis OLS (zonder Fixed Effects)
# ------------------------------------------------------------------------------
model_ols <- feols(CAR ~ Tightness + Regulation + Supervision, 
                   data = analysis_data)

# ------------------------------------------------------------------------------
# MODEL 2: Bank Fixed Effects
# Hiermee controleer je voor alle tijd-onveranderlijke bankkarakteristieken
# ------------------------------------------------------------------------------
model_bank_fe <- feols(CAR ~ Tightness + Regulation + Supervision | Bank, 
                       data = analysis_data)

# ------------------------------------------------------------------------------
# MODEL 3: Bank & Jaar Fixed Effects
# Dit is het meest robuuste model: het controleert voor banken én globale tijds-trends
# ------------------------------------------------------------------------------
model_full_fe <- feols(CAR ~ Tightness + Regulation + Supervision | Bank + Year, 
                       data = analysis_data)

# ------------------------------------------------------------------------------
# RESULTATEN VERGELIJKEN
# ------------------------------------------------------------------------------

# We gebruiken 'etable' om een mooie academische tabel te maken
# De standaardfouten worden automatisch geclusterd op 'Bank' niveau (zeer belangrijk!)
summary_table <- etable(model_ols, model_bank_fe, model_full_fe, 
                        cluster = ~Bank,
                        headers = c("Basis OLS", "Bank FE", "Bank & Year FE"))

print(summary_table)

# ------------------------------------------------------------------------------
# VISUALISATIE: COEFFICIENT PLOT
# ------------------------------------------------------------------------------
# Hiermee zie je in één oogopslag welke variabelen significant zijn
coefplot(model_full_fe, 
         main = "Impact van Speech Inhoud op Bank Returns (CAR [-1, +1])",
         dict = c(Tightness = "Tightness (Hawkishness)", 
                  Regulation = "Regulation Focus", 
                  Supervision = "Supervision Focus"))