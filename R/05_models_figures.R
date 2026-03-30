library(fixest)

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

# De kolom year_month aanmaken (als je dat nog niet gedaan had)
final_esm_data$year_month <- format(as.Date(final_esm_data$SpeechDate), "%Y-%m")

# ONDERZOEKSVRAAG1

m1.1_direction <- feols(CAR ~ Regulation + Supervision + 
                           ROA + log(TotalAssets) + CapProxy | 
                           Ticker + year_month, 
                         data = final_esm_data)
summary(m1.1_direction)

m1.2_intensity <- feols(abs_CAR ~ Regulation + Supervision + 
                           ROA + log(TotalAssets) + CapProxy | 
                           Ticker + year_month, 
                         data = final_esm_data)
summary(m1.2_intensity)

# ONDERZOEKSVRAAG 2

m2.1_tightness <- feols(CAR ~ Regulation + Supervision + Tightness + 
                          ROA + log(TotalAssets) + CapProxy | 
                          Ticker + year_month, 
                        data = final_esm_data)
summary(m2.1_tightness)

m2.2_tightness_interaction <- feols(CAR ~ Regulation * Tightness + Supervision * Tightness +
                            ROA + log(TotalAssets) + CapProxy | 
                            Ticker + year_month, 
                          data = final_esm_data)
summary(m2.2_tightness_interaction)

# ONDERZOEKSVRAAG 3

m3.1_power_intensity <- feols(abs_CAR ~ Regulation * Has_Supervisory_Power + 
                                Supervision * Has_Supervisory_Power + 
                                ROA + log(TotalAssets) + CapProxy | 
                                Ticker + year_month, 
                              data = final_esm_data)
summary(m3.1_power_intensity)

m3.2_power_direction <- feols(CAR ~ Regulation * Has_Supervisory_Power + 
                                         Supervision * Has_Supervisory_Power + 
                                         Tightness * Has_Supervisory_Power + 
                                         ROA + log(TotalAssets) + CapProxy | 
                                         Ticker + year_month, 
                                       data = final_esm_data)
summary(m3.2_power_direction)

m3.3_triple_power <- feols(CAR ~ Supervision * Tightness * Has_Supervisory_Power + 
                                Regulation * Tightness * Has_Supervisory_Power +
                                ROA + log(TotalAssets) + CapProxy | 
                                Ticker + year_month, 
                              data = final_esm_data)
summary(m3.3_power_direction)

# ONDERZOEKSVRAAG 4

m4.1_gsib_intensity <- feols(abs_CAR ~ Regulation * is_GSIB + 
                               Supervision * is_GSIB + 
                               ROA + log(TotalAssets) + CapProxy | 
                               Ticker + year_month, 
                             data = final_esm_data)
summary(m4.1_gsib_intensity)

m4.2_gsib_direction <- feols(CAR ~ Regulation * is_GSIB + 
                                      Supervision * is_GSIB + 
                                      Tightness * is_GSIB +
                                      ROA + log(TotalAssets) + CapProxy | 
                                      Ticker + year_month, 
                                    data = final_esm_data)
summary(m4.2_gsib_direction)

m4.3_triple_gsib <- feols(CAR ~ Supervision * Tightness * is_GSIB + 
                            Regulation * Tightness * is_GSIB + 
                            ROA + log(TotalAssets) + CapProxy | 
                            Ticker + year_month, 
                          data = final_esm_data)
summary(m4.3_triple_gsib)

# ONDERZOEKSVRAAG 5

m5.1_crisis_relevance <- feols(abs_CAR ~ Regulation * crisis + 
                                 Supervision * crisis + 
                                 ROA + log(TotalAssets) + CapProxy | 
                                 Ticker + year_month, 
                               data = final_esm_data)

summary(m5.1_crisis_relevance)

m5.2_crisis_sensitivity <- feols(CAR ~ Regulation * crisis + 
                                   Supervision * crisis + 
                                   Tightness * crisis + 
                                   ROA + log(TotalAssets) + CapProxy | 
                                   Ticker + year_month, 
                                 data = final_esm_data)

summary(m5.2_crisis_sensitivity)

m5.3_crisis_triple <- feols(CAR ~ Supervision * Tightness * crisis + 
                              Regulation * Tightness * crisis + 
                              ROA + log(TotalAssets) + CapProxy | 
                              Ticker + year_month, 
                            data = final_esm_data)

summary(m5.3_crisis_triple)

m5.4_vix_relevance <- feols(abs_CAR ~ Regulation * VIX_Level + 
                              Supervision * VIX_Level +
                              ROA + log(TotalAssets) + CapProxy | 
                              Ticker + year_month, 
                            data = final_esm_data)
summary(m5.4_vix_relevance)

m5.5_vix_sensitivity <- feols(CAR ~ Regulation * VIX_Level + 
                                Supervision * VIX_Level + 
                                Tightness * VIX_Level + 
                                ROA + log(TotalAssets) + CapProxy | 
                                Ticker + year_month, 
                              data = final_esm_data)
summary(m5.5_vix_sensitivity)

m5.6_vix_triple <- feols(CAR ~ Supervision * Tightness * VIX_Level + 
                           Regulation * Tightness * VIX_Level + 
                           ROA + log(TotalAssets) + CapProxy | 
                           Ticker + year_month, 
                         data = final_esm_data)

summary(m5.6_vix_triple)

# SAMENVATTENDE TABEL VAN ALLE RESULTATEN

library(modelsummary)
library(kableExtra)

modellen_h1 <- list(
  "M1.1 (CAR)" = m1.1_direction,
  "M1.2 (|CAR|)" = m1.2_intensity,
  "M2.1 (CAR)" = m2.1_tightness,
  "M2.2 (CAR)" = m2.2_tightness_interaction,
  "M3.1 (|CAR|)" = m3.1_power_intensity,
  "M3.2 (CAR)" = m3.2_power_direction,
  "M3.3 (CAR)" = m3.3_triple_power,
  "M4.1 (|CAR|)" = m4.1_gsib_intensity,
  "M4.2 (CAR)" = m4.2_gsib_direction,
  "M4.3 (CAR)" = m4.3_triple_gsib,
  "M5.1 (|CAR|)" = m5.1_crisis_relevance,
  "M5.2 (CAR)" = m5.2_crisis_sensitivity,
  "M5.3 (CAR)" = m5.3_crisis_triple,
  "M5.4 (|CAR|)" = m5.4_vix_relevance,
  "M5.5 (CAR)" = m5.5_vix_sensitivity,
  "M5.6 (CAR)" = m5.6_vix_triple
)

# Genereer de tabel als 'markdown' (verschijnt in je console onderaan)
modelsummary(modellen_h1, 
             stars = TRUE, 
             output = "markdown",
             fmt = 4,
             coef_map = c("Regulation" = "Regulation", 
                          "Supervision" = "Supervision",
                          "ROA" = "ROA",
                          "log(TotalAssets)" = "Bank size (log)",
                          "CapProxy" = "Capital Proxy"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"))


# Output in Viewer rechts onderaan
modelsummary(modellen_h1, 
             stars = TRUE, 
             output = "kableExtra",
             fmt = 4, 
             coef_map = c("Regulation" = "Regulation", 
                          "Supervision" = "Supervision",
                          "ROA" = "ROA",
                          "log(TotalAssets)" = "Bank size (log)",
                          "CapProxy" = "Capital Proxy"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"))
