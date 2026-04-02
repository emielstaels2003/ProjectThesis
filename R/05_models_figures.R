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

final_esm_data_FF3 <- final_esm_data_FF3 %>%
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

final_esm_lagged_data <- final_esm_lagged_data %>%
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

# De kolom year_month aanmaken (als je dat nog niet gedaan had)
final_esm_data_FF3$year_month <- format(as.Date(final_esm_data_FF3$SpeechDate), "%Y-%m")

# De kolom year_month aanmaken (als je dat nog niet gedaan had)
final_esm_lagged_data$year_month <- format(as.Date(final_esm_lagged_data$SpeechDate), "%Y-%m")

######## OBV MARKET MODEL

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
summary(m3.3_triple_power)

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

########## OBV FAMA FRENCH

# ONDERZOEKSVRAAG 1

ff3_r1_direction <- feols(CAR ~ Regulation + Supervision + 
                            ROA + log(TotalAssets) + CapProxy | 
                            Ticker + year_month, 
                          data = final_esm_data_FF3)
summary(ff3_r1_direction)

ff3_r1_intensity <- feols(abs_CAR ~ Regulation + Supervision + 
                            ROA + log(TotalAssets) + CapProxy | 
                            Ticker + year_month, 
                          data = final_esm_data_FF3)
summary(ff3_r1_intensity)

# ONDERZOEKSVRAAG 2

ff3_r2_tightness <- feols(CAR ~ Regulation + Supervision + Tightness + 
                            ROA + log(TotalAssets) + CapProxy | 
                            Ticker + year_month, 
                          data = final_esm_data_FF3)
summary(ff3_r2_tightness)

ff3_r2_tightness_interact <- feols(CAR ~ Regulation * Tightness + Supervision * Tightness +
                                     ROA + log(TotalAssets) + CapProxy | 
                                     Ticker + year_month, 
                                   data = final_esm_data_FF3)
summary(ff3_r2_tightness_interact)

# ONDERZOEKSVRAAG 3

ff3_r3_power_intensity <- feols(abs_CAR ~ Regulation * Has_Supervisory_Power + 
                                  Supervision * Has_Supervisory_Power + 
                                  ROA + log(TotalAssets) + CapProxy | 
                                  Ticker + year_month, 
                                data = final_esm_data_FF3)
summary(ff3_r3_power_intensity)

ff3_r3_power_direction <- feols(CAR ~ Regulation * Has_Supervisory_Power + 
                                  Supervision * Has_Supervisory_Power + 
                                  Tightness * Has_Supervisory_Power + 
                                  ROA + log(TotalAssets) + CapProxy | 
                                  Ticker + year_month, 
                                data = final_esm_data_FF3)
summary(ff3_r3_power_direction)

ff3_r3_triple_power <- feols(CAR ~ Supervision * Tightness * Has_Supervisory_Power + 
                               Regulation * Tightness * Has_Supervisory_Power +
                               ROA + log(TotalAssets) + CapProxy | 
                               Ticker + year_month, 
                             data = final_esm_data_FF3)
summary(ff3_r3_triple_power)

# ONDERZOEKSVRAAG 4

ff3_r4_gsib_intensity <- feols(abs_CAR ~ Regulation * is_GSIB + 
                                 Supervision * is_GSIB + 
                                 ROA + log(TotalAssets) + CapProxy | 
                                 Ticker + year_month, 
                               data = final_esm_data_FF3)
summary(ff3_r4_gsib_intensity)

ff3_r4_gsib_direction <- feols(CAR ~ Regulation * is_GSIB + 
                                 Supervision * is_GSIB + 
                                 Tightness * is_GSIB +
                                 ROA + log(TotalAssets) + CapProxy | 
                                 Ticker + year_month, 
                               data = final_esm_data_FF3)
summary(ff3_r4_gsib_direction)

ff3_r4_triple_gsib <- feols(CAR ~ Supervision * Tightness * is_GSIB + 
                              Regulation * Tightness * is_GSIB + 
                              ROA + log(TotalAssets) + CapProxy | 
                              Ticker + year_month, 
                            data = final_esm_data_FF3)
summary(ff3_r4_triple_gsib)

# ONDERZOEKSVRAAG 5

ff3_r5_crisis_relevance <- feols(abs_CAR ~ Regulation * crisis + 
                                   Supervision * crisis + 
                                   ROA + log(TotalAssets) + CapProxy | 
                                   Ticker + year_month, 
                                 data = final_esm_data_FF3)
summary(ff3_r5_crisis_relevance)

ff3_r5_crisis_sensitivity <- feols(CAR ~ Regulation * crisis + 
                                     Supervision * crisis + 
                                     Tightness * crisis + 
                                     ROA + log(TotalAssets) + CapProxy | 
                                     Ticker + year_month, 
                                   data = final_esm_data_FF3)
summary(ff3_r5_crisis_sensitivity)

ff3_r5_crisis_triple <- feols(CAR ~ Supervision * Tightness * crisis + 
                                Regulation * Tightness * crisis + 
                                ROA + log(TotalAssets) + CapProxy | 
                                Ticker + year_month, 
                              data = final_esm_data_FF3)
summary(ff3_r5_crisis_triple)

ff3_r5_vix_relevance <- feols(abs_CAR ~ Regulation * VIX_Level + 
                                Supervision * VIX_Level +
                                ROA + log(TotalAssets) + CapProxy | 
                                Ticker + year_month, 
                              data = final_esm_data_FF3)
summary(ff3_r5_vix_relevance)

ff3_r5_vix_sensitivity <- feols(CAR ~ Regulation * VIX_Level + 
                                  Supervision * VIX_Level + 
                                  Tightness * VIX_Level + 
                                  ROA + log(TotalAssets) + CapProxy | 
                                  Ticker + year_month, 
                                data = final_esm_data_FF3)
summary(ff3_r5_vix_sensitivity)

ff3_r5_vix_triple <- feols(CAR ~ Supervision * Tightness * VIX_Level + 
                             Regulation * Tightness * VIX_Level + 
                             ROA + log(TotalAssets) + CapProxy | 
                             Ticker + year_month, 
                           data = final_esm_data_FF3)
summary(ff3_r5_vix_triple)

########## OBV LAGGED MARKET MODEL

# ONDERZOEKSVRAAG 1

lag_m1_1_direction <- feols(CAR ~ Regulation + Supervision + 
                              ROA + log(TotalAssets) + CapProxy | 
                              Ticker + year_month, 
                            data = final_esm_lagged_data)
summary(lag_m1_1_direction)

lag_m1_2_intensity <- feols(abs_CAR ~ Regulation + Supervision + 
                              ROA + log(TotalAssets) + CapProxy | 
                              Ticker + year_month, 
                            data = final_esm_lagged_data)
summary(lag_m1_2_intensity)


# ONDERZOEKSVRAAG 2

lag_m2_1_tightness <- feols(CAR ~ Regulation + Supervision + Tightness + 
                              ROA + log(TotalAssets) + CapProxy | 
                              Ticker + year_month, 
                            data = final_esm_lagged_data)
summary(lag_m2_1_tightness)

lag_m2_2_tightness_interaction <- feols(CAR ~ Regulation * Tightness + Supervision * Tightness +
                                          ROA + log(TotalAssets) + CapProxy | 
                                          Ticker + year_month, 
                                        data = final_esm_lagged_data)
summary(lag_m2_2_tightness_interaction)

# ONDERZOEKSVRAAG 3

lag_m3_1_power_intensity <- feols(abs_CAR ~ Regulation * Has_Supervisory_Power + 
                                    Supervision * Has_Supervisory_Power + 
                                    ROA + log(TotalAssets) + CapProxy | 
                                    Ticker + year_month, 
                                  data = final_esm_lagged_data)
summary(lag_m3_1_power_intensity)

lag_m3_2_power_direction <- feols(CAR ~ Regulation * Has_Supervisory_Power + 
                                    Supervision * Has_Supervisory_Power + 
                                    Tightness * Has_Supervisory_Power + 
                                    ROA + log(TotalAssets) + CapProxy | 
                                    Ticker + year_month, 
                                  data = final_esm_lagged_data)
summary(lag_m3_2_power_direction)


lag_m3_3_triple_power <- feols(CAR ~ Supervision * Tightness * Has_Supervisory_Power + 
                                 Regulation * Tightness * Has_Supervisory_Power +
                                 ROA + log(TotalAssets) + CapProxy | 
                                 Ticker + year_month, 
                               data = final_esm_lagged_data)
summary(lag_m3_3_triple_power)

# ONDERZOEKSVRAAG 4

lag_m4_1_gsib_intensity <- feols(abs_CAR ~ Regulation * is_GSIB + 
                                   Supervision * is_GSIB + 
                                   ROA + log(TotalAssets) + CapProxy | 
                                   Ticker + year_month, 
                                 data = final_esm_lagged_data)
summary(lag_m4_1_gsib_intensity)

lag_m4_2_gsib_direction <- feols(CAR ~ Regulation * is_GSIB + 
                                   Supervision * is_GSIB + 
                                   Tightness * is_GSIB +
                                   ROA + log(TotalAssets) + CapProxy | 
                                   Ticker + year_month, 
                                 data = final_esm_lagged_data)
summary(lag_m4_2_gsib_direction)

lag_m4_3_triple_gsib <- feols(CAR ~ Supervision * Tightness * is_GSIB + 
                                Regulation * Tightness * is_GSIB + 
                                ROA + log(TotalAssets) + CapProxy | 
                                Ticker + year_month, 
                              data = final_esm_lagged_data)
summary(lag_m4_3_triple_gsib)

# ONDERZOEKSVRAAG 5

lag_m5_1_crisis_relevance <- feols(abs_CAR ~ Regulation * crisis + 
                                     Supervision * crisis + 
                                     ROA + log(TotalAssets) + CapProxy | 
                                     Ticker + year_month, 
                                   data = final_esm_lagged_data)

summary(lag_m5_1_crisis_relevance)

lag_m5_2_crisis_sensitivity <- feols(CAR ~ Regulation * crisis + 
                                       Supervision * crisis + 
                                       Tightness * crisis + 
                                       ROA + log(TotalAssets) + CapProxy | 
                                       Ticker + year_month, 
                                     data = final_esm_lagged_data)

summary(lag_m5_2_crisis_sensitivity)

lag_m5_3_crisis_triple <- feols(CAR ~ Supervision * Tightness * crisis + 
                                  Regulation * Tightness * crisis + 
                                  ROA + log(TotalAssets) + CapProxy | 
                                  Ticker + year_month, 
                                data = final_esm_lagged_data)

summary(lag_m5_3_crisis_triple)

lag_m5_4_vix_relevance <- feols(abs_CAR ~ Regulation * VIX_Level + 
                                  Supervision * VIX_Level +
                                  ROA + log(TotalAssets) + CapProxy | 
                                  Ticker + year_month, 
                                data = final_esm_lagged_data)
summary(lag_m5_4_vix_relevance)

lag_m5_5_vix_sensitivity <- feols(CAR ~ Regulation * VIX_Level + 
                                    Supervision * VIX_Level + 
                                    Tightness * VIX_Level + 
                                    ROA + log(TotalAssets) + CapProxy | 
                                    Ticker + year_month, 
                                  data = final_esm_lagged_data)
summary(lag_m5_5_vix_sensitivity)

lag_m5_6_vix_triple <- feols(CAR ~ Supervision * Tightness * VIX_Level + 
                               Regulation * Tightness * VIX_Level + 
                               ROA + log(TotalAssets) + CapProxy | 
                               Ticker + year_month, 
                             data = final_esm_lagged_data)

summary(lag_m5_6_vix_triple)

# SAMENVATTENDE TABEL VAN ALLE RESULTATEN PER ONDERZOEKSVRAAG

library(modelsummary)
library(kableExtra)

O1 <- list(
  "Market |CAR|" = m1.2_intensity,
  "FF |CAR|" = ff3_r1_intensity,
  "lagged |CAR|" = lag_m1_2_intensity,
  "Market CAR" = m1.1_direction,
  "FF CAR" = ff3_r1_direction,
  "lagged CAR" = lag_m1_1_direction
)
# Genereer de tabel als 'markdown' (verschijnt in je console onderaan)
modelsummary(O1, 
             stars = TRUE, 
             output = "markdown",
             fmt = 4,
             coef_map = c("Regulation" = "Regulation", 
                          "Supervision" = "Supervision",
                          "ROA" = "ROA",
                          "log(TotalAssets)" = "Total Assets (log)",
                          "CapProxy" = "Capital Proxy"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"))
# Output in Viewer rechts onderaan
modelsummary(O1, 
             stars = TRUE, 
             output = "kableExtra",
             fmt = 4, 
             coef_map = c("Regulation" = "Regulation", 
                          "Supervision" = "Supervision",
                          "ROA" = "ROA",
                          "log(TotalAssets)" = "Bank size (log)",
                          "CapProxy" = "Capital Proxy"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"))

O2 <- list(
  "Market CAR" = m2.1_tightness,
  "FF CAR" = ff3_r2_tightness,
  "lagged CAR" = lag_m2_1_tightness,
  "Market CAR interaction" = m2.2_tightness_interaction,
  "FF CAR interaction" = ff3_r2_tightness_interact,
  "lagged CAR interaction" = lag_m2_2_tightness_interaction
)

# Genereer de tabel als 'markdown' (verschijnt in je console onderaan)
modelsummary(O2, 
             stars = TRUE, 
             output = "markdown",
             fmt = 4,
             coef_map = c(
               "Regulation" = "Regulation",
               "Supervision" = "Supervision",
               "Tightness" = "Tightness (Tone)",
               "ROA" = "ROA",
               "log(TotalAssets)" = "Bank size (log)",
               "CapProxy" = "Capital Proxy",
               
               # Interacties voor Model 2.2
               "Regulation:Tightness" = "Regulation × Tightness",
               "Tightness:Supervision" = "Supervision × Tightness"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"))
# Output in Viewer rechts onderaan
modelsummary(O2, 
             stars = TRUE, 
             output = "kableExtra",
             fmt = 4, 
             coef_map = c(
               "Regulation" = "Regulation",
               "Supervision" = "Supervision",
               "Tightness" = "Tightness (Tone)",
               "ROA" = "ROA",
               "log(TotalAssets)" = "Bank size (log)",
               "CapProxy" = "Capital Proxy",
               
               # Interacties voor Model 2.2
               "Regulation:Tightness" = "Regulation × Tightness",
               "Tightness:Supervision" = "Supervision × Tightness"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"))

O3 <- list(
  "Market |CAR|" = m3.1_power_intensity,
  "FF |CAR|" = ff3_r3_power_intensity,
  "lagged |CAR|" = lag_m3_1_power_intensity,
  "Market Double CAR" = m3.2_power_direction,
  "FF Double CAR" = ff3_r3_power_direction,
  "Lagged Double CAR" = lag_m3_2_power_direction,
  "Market Triple CAR" = m3.3_triple_power,
  "FF Triple CAR" = ff3_r3_triple_power,
  "Lagged Tripple CAR" = lag_m3_3_triple_power
)

# Genereer de tabel als 'markdown' (verschijnt in je console onderaan)
modelsummary(O3, 
             stars = TRUE, 
             output = "markdown",
             fmt = 4,
             coef_map = c(
               "Regulation" = "Regulation",
               "Supervision" = "Supervision",
               "Has_Supervisory_Power" = "Supervisory Power (Dummy)",
               "Tightness" = "Tightness (Tone)",
               "ROA" = "ROA",
               "log(TotalAssets)" = "Bank size (log)",
               "CapProxy" = "Capital Proxy",
               
               # Interacties voor Model 1 & 2
               "Regulation:Has_Supervisory_Power" = "Regulation × Power",
               "Has_Supervisory_Power:Regulation" = "Regulation × Power",
               
               # Supervision x Power
               "Supervision:Has_Supervisory_Power" = "Supervision × Power",
               "Has_Supervisory_Power:Supervision" = "Supervision × Power",
               
               # Tightness x Power
               "Tightness:Has_Supervisory_Power" = "Tightness × Power",
               "Has_Supervisory_Power:Tightness" = "Tightness × Power",

               
               # Triple Interacties voor Model 3
               "Regulation:Tightness" = "Regulation × Tightness",
               "Tightness:Regulation" = "Regulation × Tightness",
               
               # Supervision x Tightness
               "Supervision:Tightness" = "Supervision × Tightness",
               "Tightness:Supervision" = "Supervision × Tightness",
               "Supervision:Tightness:Has_Supervisory_Power" = "Supervision × Tightness × Power",
               "Tightness:Has_Supervisory_Power:Regulation" = "Regulation × Tightness × Power"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"))
# Output in Viewer rechts onderaan
modelsummary(O3, 
             stars = TRUE, 
             output = "kableExtra",
             fmt = 4, 
             coef_map = c(
               "Regulation" = "Regulation",
               "Supervision" = "Supervision",
               "Has_Supervisory_Power" = "Supervisory Power (Dummy)",
               "Tightness" = "Tightness (Tone)",
               "ROA" = "ROA",
               "log(TotalAssets)" = "Bank size (log)",
               "CapProxy" = "Capital Proxy",
               
               # Interacties voor Model 1 & 2
               "Regulation:Has_Supervisory_Power" = "Regulation × Power",
               "Has_Supervisory_Power:Regulation" = "Regulation × Power",
               
               # Supervision x Power
               "Supervision:Has_Supervisory_Power" = "Supervision × Power",
               "Has_Supervisory_Power:Supervision" = "Supervision × Power",
               
               # Tightness x Power
               "Tightness:Has_Supervisory_Power" = "Tightness × Power",
               "Has_Supervisory_Power:Tightness" = "Tightness × Power",
               
               
               # Triple Interacties voor Model 3
               "Regulation:Tightness" = "Regulation × Tightness",
               "Tightness:Regulation" = "Regulation × Tightness",
               
               # Supervision x Tightness
               "Supervision:Tightness" = "Supervision × Tightness",
               "Tightness:Supervision" = "Supervision × Tightness",
               "Supervision:Tightness:Has_Supervisory_Power" = "Supervision × Tightness × Power",
               "Tightness:Has_Supervisory_Power:Regulation" = "Regulation × Tightness × Power"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"))

O4 <- list(
  "Market |CAR|" = m4.1_gsib_intensity,
  "FF |CAR|" = ff3_r4_gsib_intensity,
  "lagged |CAR|" = lag_m4_1_gsib_intensity,
  "Market Double CAR" = m4.2_gsib_direction,
  "FF Double CAR" = ff3_r4_gsib_direction,
  "Lagged Double CAR" = lag_m4_2_gsib_direction,
  "Market Triple CAR" = m4.3_triple_gsib,
  "FF Triple CAR" = ff3_r4_triple_gsib,
  "Lagged Tripple CAR" = lag_m4_3_triple_gsib
)

# Genereer de tabel als 'markdown' (verschijnt in je console onderaan)
modelsummary(O4, 
             stars = TRUE, 
             output = "markdown",
             fmt = 4,
             coef_map = c(
               "Regulation" = "Regulation",
               "Supervision" = "Supervision",
               "is_GSIB" = "G-SIB Status (Dummy)",
               "Tightness" = "Tightness (Tone)",
               "ROA" = "ROA",
               "log(TotalAssets)" = "Bank size (log)",
               "CapProxy" = "Capital Proxy",
               
               # Interacties voor Model 4.1 & 4.2
               "Regulation:is_GSIB" = "Regulation × G-SIB",
               "is_GSIB:Regulation" = "Regulation × G-SIB",
               
               # Supervision x G-SIB
               "Supervision:is_GSIB" = "Supervision × G-SIB",
               "is_GSIB:Supervision" = "Supervision × G-SIB",
               
               # Tightness x G-SIB
               "Tightness:is_GSIB" = "Tightness × G-SIB",
               "is_GSIB:Tightness" = "Tightness × G-SIB",
               
               # Triple Interacties voor Model 4.3 (Let op de volgorde van R!)
               "Regulation:Tightness" = "Regulation × Tightness",
               "Tightness:Regulation" = "Regulation × Tightness",
               
               # Supervision x Tightness
               "Supervision:Tightness" = "Supervision × Tightness",
               "Tightness:Supervision" = "Supervision × Tightness",
               
               "Supervision:Tightness:is_GSIB" = "Supervision × Tightness × G-SIB",
               "Tightness:is_GSIB:Regulation" = "Regulation × Tightness × G-SIB"),
          gof_map = c("nobs", "r.squared", "adj.r.squared"))
# Output in Viewer rechts onderaan
modelsummary(O4, 
             stars = TRUE, 
             output = "kableExtra",
             fmt = 4, 
             coef_map = c(
               "Regulation" = "Regulation",
               "Supervision" = "Supervision",
               "is_GSIB" = "G-SIB Status (Dummy)",
               "Tightness" = "Tightness (Tone)",
               "ROA" = "ROA",
               "log(TotalAssets)" = "Bank size (log)",
               "CapProxy" = "Capital Proxy",
               
               # Interacties voor Model 4.1 & 4.2
               "Regulation:is_GSIB" = "Regulation × G-SIB",
               "is_GSIB:Regulation" = "Regulation × G-SIB",
               
               # Supervision x G-SIB
               "Supervision:is_GSIB" = "Supervision × G-SIB",
               "is_GSIB:Supervision" = "Supervision × G-SIB",
               
               # Tightness x G-SIB
               "Tightness:is_GSIB" = "Tightness × G-SIB",
               "is_GSIB:Tightness" = "Tightness × G-SIB",
               
               # Triple Interacties voor Model 4.3 (Let op de volgorde van R!)
               "Regulation:Tightness" = "Regulation × Tightness",
               "Tightness:Regulation" = "Regulation × Tightness",
               
               # Supervision x Tightness
               "Supervision:Tightness" = "Supervision × Tightness",
               "Tightness:Supervision" = "Supervision × Tightness",
               
               "Supervision:Tightness:is_GSIB" = "Supervision × Tightness × G-SIB",
               "Tightness:is_GSIB:Regulation" = "Regulation × Tightness × G-SIB"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"))

O5a <- list(
  "Market |CAR|" = m5.1_crisis_relevance,
  "FF |CAR|" = ff3_r5_crisis_relevance,
  "lagged |CAR|" = lag_m5_1_crisis_relevance,
  "Market Double CAR" = m5.2_crisis_sensitivity,
  "FF Double CAR" = ff3_r5_crisis_sensitivity,
  "Lagged Double CAR" = lag_m5_2_crisis_sensitivity,
  "Market Triple CAR" = m5.3_crisis_triple,
  "FF Triple CAR" = ff3_r5_crisis_triple,
  "Lagged Tripple CAR" = lag_m5_3_crisis_triple
)

# Genereer de tabel als 'markdown' (verschijnt in je console onderaan)
modelsummary(O5a, 
             stars = TRUE, 
             output = "markdown",
             fmt = 4,
             coef_map = c(
               "Regulation" = "Regulation",
               "Supervision" = "Supervision",
               "crisis" = "Crisis (Dummy)",
               "Tightness" = "Tightness (Tone)",
               "ROA" = "ROA",
               "log(TotalAssets)" = "Bank size (log)",
               "CapProxy" = "Capital Proxy",
               
               # Interacties voor Model 5.1 & 5.2
               "Regulation:crisis" = "Regulation × Crisis",
               "crisis:Regulation" = "Regulation × Crisis",
               
               # Supervision x Crisis
               "Supervision:crisis" = "Supervision × Crisis",
               "crisis:Supervision" = "Supervision × Crisis",
               
               # Tightness x Crisis
               "Tightness:crisis" = "Tightness × Crisis",
               "crisis:Tightness" = "Tightness × Crisis",

               
               # Triple Interacties voor Model 5.3
               "Regulation:Tightness" = "Regulation × Tightness",
               "Tightness:Regulation" = "Regulation × Tightness",
               
               # Supervision x Tightness
               "Supervision:Tightness" = "Supervision × Tightness",
               "Tightness:Supervision" = "Supervision × Tightness",
               
               "Supervision:Tightness:crisis" = "Supervision × Tightness × Crisis",
               "Tightness:crisis:Regulation" = "Regulation × Tightness × Crisis"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"))
# Output in Viewer rechts onderaan
modelsummary(O5a, 
             stars = TRUE, 
             output = "kableExtra",
             fmt = 4, 
             coef_map = c(
               "Regulation" = "Regulation",
               "Supervision" = "Supervision",
               "crisis" = "Crisis (Dummy)",
               "Tightness" = "Tightness (Tone)",
               "ROA" = "ROA",
               "log(TotalAssets)" = "Bank size (log)",
               "CapProxy" = "Capital Proxy",
               
               # Interacties voor Model 5.1 & 5.2
               "Regulation:crisis" = "Regulation × Crisis",
               "crisis:Regulation" = "Regulation × Crisis",
               
               # Supervision x Crisis
               "Supervision:crisis" = "Supervision × Crisis",
               "crisis:Supervision" = "Supervision × Crisis",
               
               # Tightness x Crisis
               "Tightness:crisis" = "Tightness × Crisis",
               "crisis:Tightness" = "Tightness × Crisis",
               
               
               # Triple Interacties voor Model 5.3
               "Regulation:Tightness" = "Regulation × Tightness",
               "Tightness:Regulation" = "Regulation × Tightness",
               
               # Supervision x Tightness
               "Supervision:Tightness" = "Supervision × Tightness",
               "Tightness:Supervision" = "Supervision × Tightness",
               
               "Supervision:Tightness:crisis" = "Supervision × Tightness × Crisis",
               "Tightness:crisis:Regulation" = "Regulation × Tightness × Crisis"),
            gof_map = c("nobs", "r.squared", "adj.r.squared"))

O5b <- list(
  "Market |CAR|" = m5.4_vix_relevance,
  "FF |CAR|" = ff3_r5_vix_relevance,
  "lagged |CAR|" = lag_m5_4_vix_relevance,
  "Market Double CAR" = m5.5_vix_sensitivity,
  "FF Double CAR" = ff3_r5_vix_sensitivity,
  "Lagged Double CAR" = lag_m5_5_vix_sensitivity,
  "Market Triple CAR" = m5.6_vix_triple,
  "FF Triple CAR" = ff3_r5_vix_triple,
  "Lagged Tripple CAR" = lag_m5_6_vix_triple
)

# Genereer de tabel als 'markdown' (verschijnt in je console onderaan)
modelsummary(O5b, 
             stars = TRUE, 
             output = "markdown",
             fmt = 4,
             coef_map = c(
               "Regulation" = "Regulation",
               "Supervision" = "Supervision",
               "VIX_Level" = "VIX Index",
               "Tightness" = "Tightness (Tone)",
               "ROA" = "ROA",
               "log(TotalAssets)" = "Bank size (log)",
               "CapProxy" = "Capital Proxy",
               
               # Interacties voor Model 5.4 & 5.5
               "Regulation:VIX_Level" = "Regulation × VIX",
               "VIX_Level:Regulation" = "Regulation × VIX",
               
               # Supervision x VIX
               "Supervision:VIX_Level" = "Supervision × VIX",
               "VIX_Level:Supervision" = "Supervision × VIX",
               
               # Tightness x VIX
               "Tightness:VIX_Level" = "Tightness × VIX",
               "VIX_Level:Tightness" = "Tightness × VIX",
               
               # Triple Interacties voor Model 5.6
               "Regulation:Tightness" = "Regulation × Tightness",
               "Tightness:Regulation" = "Regulation × Tightness",
               
               # Supervision x Tightness
               "Supervision:Tightness" = "Supervision × Tightness",
               "Tightness:Supervision" = "Supervision × Tightness",
               
               "Supervision:Tightness:VIX_Level" = "Supervision × Tightness × VIX",
               "Tightness:VIX_Level:Regulation" = "Regulation × Tightness × VIX"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"))

# Output in Viewer rechts onderaan
modelsummary(O5b, 
             stars = TRUE, 
             output = "kableExtra",
             fmt = 4, 
             coef_map = c(
               "Regulation" = "Regulation",
               "Supervision" = "Supervision",
               "VIX_Level" = "VIX Index",
               "Tightness" = "Tightness (Tone)",
               "ROA" = "ROA",
               "log(TotalAssets)" = "Bank size (log)",
               "CapProxy" = "Capital Proxy",
               
               # Interacties voor Model 5.4 & 5.5
               "Regulation:VIX_Level" = "Regulation × VIX",
               "VIX_Level:Regulation" = "Regulation × VIX",
               
               # Supervision x VIX
               "Supervision:VIX_Level" = "Supervision × VIX",
               "VIX_Level:Supervision" = "Supervision × VIX",
               
               # Tightness x VIX
               "Tightness:VIX_Level" = "Tightness × VIX",
               "VIX_Level:Tightness" = "Tightness × VIX",
               
               # Triple Interacties voor Model 5.6
               "Regulation:Tightness" = "Regulation × Tightness",
               "Tightness:Regulation" = "Regulation × Tightness",
               
               # Supervision x Tightness
               "Supervision:Tightness" = "Supervision × Tightness",
               "Tightness:Supervision" = "Supervision × Tightness",
               
               "Supervision:Tightness:VIX_Level" = "Supervision × Tightness × VIX",
               "Tightness:VIX_Level:Regulation" = "Regulation × Tightness × VIX"),
             gof_map = c("nobs", "r.squared", "adj.r.squared"))


