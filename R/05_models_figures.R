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
  #  Supervision = as.factor(Supervision),
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
 #   Regulation = as.factor(Regulation),
#   Supervision = as.factor(Supervision),
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
    # Supervision = as.factor(Supervision),
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
library(fixest)
wald(m1.1_direction, keep = c("Regulation", "Supervision"))

##basismodel -1,1
m1.2_intensity <- feols(abs_CAR ~ Regulation + Supervision + 
                            ROA + log(TotalAssets) + CapProxy | 
                            Ticker + year_month, 
                          data = final_esm_data)
summary(m1.2_intensity)

#zonder bank controls
m1.2_intensity_withoutBC <- feols(abs_CAR ~ Regulation + Supervision | 
                          Ticker + year_month, 
                        data = final_esm_data)
summary(m1.2_intensity_withoutBC)

#zonder fixed effects
m1.2_intensity_withoutFE <- feols(abs_CAR ~ Regulation + Supervision + ROA + log(TotalAssets) + CapProxy,
                                  data = final_esm_data)
summary(m1.2_intensity_withoutFE)

# zonder BC en zonder FE
m1.2_intensity_withoutBCFE <- feols(abs_CAR ~ Regulation + Supervision, 
                        data = final_esm_data)
summary(m1.2_intensity_withoutBCFE)

library(modelsummary)
library(kableExtra)

# Maak een lijst van de modellen uit je screenshot
models_comparison <- list(
  "Without Bank Controls"   = m1.2_intensity_withoutBC,
  "Without Fixed Effects"   = m1.2_intensity_withoutFE,
  "Without Bank Controls & Fixed Effects" = m1.2_intensity_withoutBCFE,
  "With Bank Controls & Fixed Effects" = m1.2_intensity
)

# Genereer de vergelijkingstabel
modelsummary(models_comparison,
             fmt = 6,
             stars = TRUE,
             coef_map = c(
               "(Intercept)" = "Constant",
               "Regulation" = "Regulation",
               "Supervision" = "Supervision",
               "ROA" = "ROA",
               "log(TotalAssets)" = "Bank Size (log)",
               "CapProxy" = "Capital Proxy"
             ),
             gof_map = c("nobs", "r.squared"),
)

direction_summary <- list("Baseline CAR" = m1.1_direction)

# Genereer de horizontale samenvattingstabel
modelsummary(direction_summary, 
             shape = model ~ term, 
             fmt = 6, 
             stars = TRUE,
             coef_map = c(
               "Regulation" = "Regulation",
               "Supervision" = "Supervision",
               "ROA" = "ROA",
               "log(TotalAssets)" = "Bank Size (log)",
               "CapProxy" = "Capital Proxy"
             ),
             gof_map = c("nobs", "r.squared")
)

m1.2_intensityCAT <- feols(abs_CAR ~ i(Regulation) + i(Supervision) + 
                          ROA + log(TotalAssets) + CapProxy | 
                          Ticker + year_month, 
                        data = final_esm_data)
summary(m1.2_intensityCAT)
modelsCAT <- list("Categorical Intensity Model" = m1.2_intensityCAT)
# Genereer de horizontale samenvattingstabel
modelsummary(modelsCAT, 
             shape = model ~ term, 
             fmt = 6, 
             stars = TRUE,
             coef_map = c(
               "Regulation::1" = "Regulation (L1)",
               "Regulation::2" = "Regulation (L2)",
               "Supervision::1" = "Supervision (L1)",
               "Supervision::2" = "Supervision (L2)",
               "ROA" = "ROA",
               "log(TotalAssets)" = "Bank Size (log)",
               "CapProxy" = "Capital Proxy"
             ),
             gof_map = c("nobs", "r.squared")
)

m1.2_intensityRechts <- feols(abs_CAR ~ Regulation + Supervision + 
                          ROA + log(TotalAssets) + CapProxy | 
                          Ticker + year_month, 
                        data = final_esm_data)
summary(m1.2_intensityRechts)
m1.2_intensityLinks <- feols(abs_CAR ~ Regulation + Supervision + 
                                ROA + log(TotalAssets) + CapProxy | 
                                Ticker + year_month, 
                              data = final_esm_data)
summary(m1.2_intensityLinks)
m1.2_intensityGroot <- feols(abs_CAR ~ Regulation + Supervision + 
                               ROA + log(TotalAssets) + CapProxy | 
                               Ticker + year_month, 
                             data = final_esm_data)
summary(m1.2_intensityGroot)

# Maak een lijst van je modellen voor de verschillende windows
# Ik heb de namen gebaseerd op je screenshot
robustness_models <- list(
  "[-1, 1]" = m1.2_intensity, # Je basismodel
  "[-1, 5]" = m1.2_intensityRechts,   # Check je exacte modelnaam in R
  "[-5, 1]" = m1.2_intensityLinks,
  "[-3, 3]" = m1.2_intensityGroot
)

# Genereer de vergelijkingstabel
modelsummary(robustness_models,
             fmt = 6,
             stars = TRUE,
             coef_map = c(
               "Regulation" = "Regulation",
               "Supervision" = "Supervision",
               "ROA" = "ROA",
               "log(TotalAssets)" = "Bank Size (log)",
               "CapProxy" = "Capital Proxy"
             ),
             gof_map = c("nobs", "r.squared"),
)

library(modelsummary)

# Create a list for the model
models <- list("Baseline CAR" = m1.1_direction)

# Generate a horizontal table (Models in rows, Variables in columns)
modelsummary(models, 
             shape = model ~ term,        # This transposes the standard layout
             fmt = 6,                     # Sets decimals to 6
             stars = TRUE,                # Adds significance stars
             coef_map = c(                # Professional labels
               "Regulation" = "Regulation",
               "Supervision" = "Supervision",
               "ROA" = "ROA",
               "log(TotalAssets)" = "Bank Size (log)",
               "CapProxy" = "Capital Proxy"
             ),
             gof_map = c("nobs", "r.squared"), # Concise summary statistics
             title = "Regression Summary")

# Create a list for the model
models <- list("Baseline |CAR|" = m1.2_intensity)

# Generate a horizontal table (Models in rows, Variables in columns)
modelsummary(models, 
             shape = model ~ term,        # This transposes the standard layout
             fmt = 6,                     # Sets decimals to 6
             stars = TRUE,                # Adds significance stars
             coef_map = c(                # Professional labels
               "Regulation" = "Regulation",
               "Supervision" = "Supervision",
               "ROA" = "ROA",
               "log(TotalAssets)" = "Bank Size (log)",
               "CapProxy" = "Capital Proxy"
             ),
             gof_map = c("nobs", "r.squared"), # Concise summary statistics
)

# ONDERZOEKSVRAAG 2

m2.1_tightness <- feols(CAR ~ Regulation + Supervision + Tightness + 
                          ROA + log(TotalAssets) + CapProxy | 
                          Ticker + year_month, 
                        data = final_esm_data)
summary(m2.1_tightness)

wald(m2.1_tightness, keep = c("Regulation", "Supervision", "Tightness"))
wald(m2.1_tightness, keep = "Regulation| Supervision | Tightness")

m2.2_tightness_interaction <- feols(CAR ~ Regulation * Tightness + Supervision * Tightness +
                            ROA + log(TotalAssets) + CapProxy | 
                            Ticker + year_month, 
                          data = final_esm_data)
summary(m2.2_tightness_interaction)

# Create a list for the model
models <- list("Tightness included" = m2.1_tightness)

# Generate a horizontal table (Models in rows, Variables in columns)
modelsummary(models, 
             shape = model ~ term,        # This transposes the standard layout
             fmt = 6,                     # Sets decimals to 6
             stars = TRUE,                # Adds significance stars
             coef_map = c(                # Professional labels
               "Regulation" = "Regulation",
               "Supervision" = "Supervision",
               "Tightness" = "Tightness",
               "ROA" = "ROA",
               "log(TotalAssets)" = "Bank Size (log)",
               "CapProxy" = "Capital Proxy"
             ),
             gof_map = c("nobs", "r.squared"), # Concise summary statistics
)

models_tightness <- list("Tightness Interaction" = m2.2_tightness_interaction)
modelsummary(models_tightness, 
             shape = model ~ term, 
             fmt = 6, 
             stars = TRUE,
             coef_map = c(
               "Regulation" = "Regulation",
               "Tightness" = "Tightness",
               "Supervision" = "Supervision",
               "ROA" = "ROA",
               "log(TotalAssets)" = "Bank Size (log)",
               "CapProxy" = "Capital Proxy",
               "Regulation:Tightness" = "Reg x Tight",
               "Tightness:Supervision" = "Sup x Tight"
             ),
             gof_map = c("nobs", "r.squared")
)

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


# Maak de lijst voor het intensiteitsmodel
models_intensity <- list("Power Intensity" = m3.1_power_intensity)

# Genereer de horizontale tabel
modelsummary(models_intensity, 
             shape = model ~ term, 
             fmt = 6, 
             stars = TRUE,
             coef_map = c(
               "Regulation" = "Regulation",
               "Supervision" = "Supervision",
               "Has_Supervisory_Power" = "Supervisory Power",
               "ROA" = "ROA",
               "log(TotalAssets)" = "Bank Size (log)",
               "CapProxy" = "Capital Proxy",
               "Regulation:Has_Supervisory_Power" = "Reg x Power",
               "Has_Supervisory_Power:Supervision" = "Sup x Power"
             ),
             gof_map = c("nobs", "r.squared")
)

models_triple <- list("TIRP" = m3.3_triple_power)

# Horizontale tabel genereren
modelsummary(models_triple, 
             shape = model ~ term, 
             fmt = 6, 
             stars = TRUE,
             coef_map = c(
               "Supervision" = "Sup",
               "Tightness" = "Tight",
               "Has_Supervisory_Power" = "Power",
               "Regulation" = "Reg",
               "ROA" = "ROA",
               "log(TotalAssets)" = "Size (log)",
               "CapProxy" = "Cap Prox",
               "Supervision:Tightness" = "Sup x Tight",
               "Supervision:Has_Supervisory_Power" = "Sup x Power",
               "Tightness:Has_Supervisory_Power" = "Tight x Power",
               "Tightness:Regulation" = "Tight x Reg",
               "Has_Supervisory_Power:Regulation" = "Power x Reg",
               "Supervision:Tightness:Has_Supervisory_Power" = "Sup x Tight x Power",
               "Tightness:Has_Supervisory_Power:Regulation" = "Reg x Tight x Power"
             ),
             gof_map = c("nobs", "r.squared")
)

# ONDERZOEKSVRAAG 4

m4.1_gsib_intensity <- feols(abs_CAR ~ Regulation * is_GSIB + 
                               Supervision * is_GSIB + 
                               ROA + log(TotalAssets) + CapProxy | 
                               Ticker + year_month, 
                             data = final_esm_data)
summary(m4.1_gsib_intensity)

models_gsib <- list("G-SIB Intensity" = m4.1_gsib_intensity)

# Genereer de horizontale tabel met groter lettertype
modelsummary(models_gsib, 
             shape = model ~ term, 
             fmt = 6, 
             stars = TRUE,
             coef_map = c(
               "Regulation" = "Regulation",
               "Supervision" = "Supervision",
               "ROA" = "ROA",
               "log(TotalAssets)" = "Bank Size (log)",
               "CapProxy" = "Capital Proxy",
               "Regulation:is_GSIB" = "Reg x GSIB",
               "is_GSIB:Supervision" = "Sup x GSIB"
             ),
             gof_map = c("nobs", "r.squared")
)

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

models_triple_gsib <- list("TIRGSIB" = m4.3_triple_gsib)

# Horizontale tabel genereren met groter lettertype
modelsummary(models_triple_gsib, 
             shape = model ~ term, 
             fmt = 6, 
             stars = TRUE,
             coef_map = c(
               "Supervision" = "Sup",
               "Tightness" = "Tight",
               "Regulation" = "Reg",
               "ROA" = "ROA",
               "log(TotalAssets)" = "Size (log)",
               "CapProxy" = "Cap Prox",
               "Supervision:Tightness" = "Sup x Tight",
               "Supervision:is_GSIB" = "Sup x GSIB",
               "Tightness:is_GSIB" = "Tight x GSIB",
               "Tightness:Regulation" = "Tight x Reg",
               "is_GSIB:Regulation" = "GSIB x Reg",
               "Supervision:Tightness:is_GSIB" = "Sup x Tight x GSIB",
               "Tightness:is_GSIB:Regulation" = "Tight x GSIB x Reg"
             ),
             gof_map = c("nobs", "r.squared")
)

# ONDERZOEKSVRAAG 5

m5.1_crisis_relevance <- feols(abs_CAR ~ Regulation * crisis + 
                                 Supervision * crisis + 
                                 ROA + log(TotalAssets) + CapProxy | 
                                 Ticker + year_month, 
                               data = final_esm_data)

summary(m5.1_crisis_relevance)

# Maak de lijst voor het crisis relevance model
models_crisis_rel <- list("Crisis Intensity" = m5.1_crisis_relevance)

# Genereer de horizontale tabel met groter lettertype
modelsummary(models_crisis_rel, 
             shape = model ~ term, 
             fmt = 6, 
             stars = TRUE,
             coef_map = c(
               "Regulation" = "Regulation",
               "crisis" = "Crisis",
               "Supervision" = "Supervision",
               "ROA" = "ROA",
               "log(TotalAssets)" = "Bank Size (log)",
               "CapProxy" = "Capital Proxy",
               "Regulation:crisis" = "Reg x Crisis",
               "crisis:Supervision" = "Sup x Crisis"
             ),
             gof_map = c("nobs", "r.squared")
)

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

models_crisis <- list("TIRC" = m5.3_crisis_triple)

# Genereer de horizontale tabel
modelsummary(models_crisis, 
             shape = model ~ term, 
             fmt = 6, 
             stars = TRUE,
             coef_map = c(
               "Supervision" = "Sup",
               "Tightness" = "Tight",
               "crisis" = "Crisis",
               "Regulation" = "Reg",
               "ROA" = "ROA",
               "log(TotalAssets)" = "Size (log)",
               "CapProxy" = "Cap Prox",
               "Supervision:Tightness" = "Sup x Tight",
               "Supervision:crisis" = "Sup x Crisis",
               "Tightness:crisis" = "Tight x Crisis",
               "Tightness:Regulation" = "Tight x Reg",
               "crisis:Regulation" = "Crisis x Reg",
               "Supervision:Tightness:crisis" = "Sup x Tight x Crisis",
               "Tightness:crisis:Regulation" = "Tight x Crisis x Reg"
             ),
             gof_map = c("nobs", "r.squared")
)

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

#### Robuustheidscheck NO CRISIS

data_no_crisis <- subset(final_esm_data, crisis == 0)

m1.1_directionNC <- feols(CAR ~ Regulation + Supervision + 
                          ROA + log(TotalAssets) + CapProxy | 
                          Ticker + year_month, 
                        data = data_no_crisis)
m1.2_intensityNC <- feols(abs_CAR ~ Regulation + Supervision + 
                          ROA + log(TotalAssets) + CapProxy | 
                          Ticker + year_month, 
                        data = data_no_crisis)

m2.1_tightnessNC <- feols(CAR ~ Regulation + Supervision + Tightness + 
                          ROA + log(TotalAssets) + CapProxy | 
                          Ticker + year_month, 
                        data = data_no_crisis)
m2.2_tightness_interactionNC <- feols(CAR ~ Regulation * Tightness + Supervision * Tightness +
                                      ROA + log(TotalAssets) + CapProxy | 
                                      Ticker + year_month, 
                                    data = data_no_crisis)

m3.1_power_intensityNC <- feols(abs_CAR ~ Regulation * Has_Supervisory_Power + 
                                Supervision * Has_Supervisory_Power + 
                                ROA + log(TotalAssets) + CapProxy | 
                                Ticker + year_month, 
                              data = data_no_crisis)
m3.2_power_directionNC <- feols(CAR ~ Regulation * Has_Supervisory_Power + 
                                Supervision * Has_Supervisory_Power + 
                                Tightness * Has_Supervisory_Power + 
                                ROA + log(TotalAssets) + CapProxy | 
                                Ticker + year_month, 
                              data = data_no_crisis)
m3.3_triple_powerNC <- feols(CAR ~ Supervision * Tightness * Has_Supervisory_Power + 
                             Regulation * Tightness * Has_Supervisory_Power +
                             ROA + log(TotalAssets) + CapProxy | 
                             Ticker + year_month, 
                           data = data_no_crisis)

m4.1_gsib_intensityNC <- feols(abs_CAR ~ Regulation * is_GSIB + 
                               Supervision * is_GSIB + 
                               ROA + log(TotalAssets) + CapProxy | 
                               Ticker + year_month, 
                             data = data_no_crisis)
m4.2_gsib_directionNC <- feols(CAR ~ Regulation * is_GSIB + 
                               Supervision * is_GSIB + 
                               Tightness * is_GSIB +
                               ROA + log(TotalAssets) + CapProxy | 
                               Ticker + year_month, 
                             data = data_no_crisis)
m4.3_triple_gsibNC <- feols(CAR ~ Supervision * Tightness * is_GSIB + 
                            Regulation * Tightness * is_GSIB + 
                            ROA + log(TotalAssets) + CapProxy | 
                            Ticker + year_month, 
                          data = data_no_crisis)

m5.1_crisis_relevanceNC <- feols(abs_CAR ~ Regulation * crisis + 
                                 Supervision * crisis + 
                                 ROA + log(TotalAssets) + CapProxy | 
                                 Ticker + year_month, 
                               data = data_no_crisis)
m5.2_crisis_sensitivityNC <- feols(CAR ~ Regulation * crisis + 
                                   Supervision * crisis + 
                                   Tightness * crisis + 
                                   ROA + log(TotalAssets) + CapProxy | 
                                   Ticker + year_month, 
                                 data = data_no_crisis)
m5.3_crisis_tripleNC <- feols(CAR ~ Supervision * Tightness * crisis + 
                              Regulation * Tightness * crisis + 
                              ROA + log(TotalAssets) + CapProxy | 
                              Ticker + year_month, 
                            data = data_no_crisis)
m5.4_vix_relevanceNC <- feols(abs_CAR ~ Regulation * VIX_Level + 
                              Supervision * VIX_Level +
                              ROA + log(TotalAssets) + CapProxy | 
                              Ticker + year_month, 
                            data = data_no_crisis)
m5.5_vix_sensitivityNC <- feols(CAR ~ Regulation * VIX_Level + 
                                Supervision * VIX_Level + 
                                Tightness * VIX_Level + 
                                ROA + log(TotalAssets) + CapProxy | 
                                Ticker + year_month, 
                              data = data_no_crisis)
m5.6_vix_tripleNC <- feols(CAR ~ Supervision * Tightness * VIX_Level + 
                           Regulation * Tightness * VIX_Level + 
                           ROA + log(TotalAssets) + CapProxy | 
                           Ticker + year_month, 
                         data = data_no_crisis)

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
  "Market |CAR| NC" = m1.2_intensityNC,
  "FF |CAR|" = ff3_r1_intensity,
  "lagged |CAR|" = lag_m1_2_intensity,
  "Market CAR" = m1.1_direction,
  "Market CAR NC" = m1.1_directionNC,
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
  "Market CAR NC" = m2.1_tightnessNC,
  "FF CAR" = ff3_r2_tightness,
  "lagged CAR" = lag_m2_1_tightness,
  "Market CAR interaction" = m2.2_tightness_interaction,
  "Market CAR interaction NC" = m2.2_tightness_interactionNC,
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
  "Market |CAR| NC" = m3.1_power_intensityNC,
  "FF |CAR|" = ff3_r3_power_intensity,
  "lagged |CAR|" = lag_m3_1_power_intensity,
  "Market Double CAR" = m3.2_power_direction,
  "Market Double CAR NC" = m3.2_power_directionNC,
  "FF Double CAR" = ff3_r3_power_direction,
  "Lagged Double CAR" = lag_m3_2_power_direction,
  "Market Triple CAR" = m3.3_triple_power,
  "Market Triple CAR NC" = m3.3_triple_powerNC,
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
  "Market |CAR| NC" = m4.1_gsib_intensityNC,
  "FF |CAR|" = ff3_r4_gsib_intensity,
  "lagged |CAR|" = lag_m4_1_gsib_intensity,
  "Market Double CAR" = m4.2_gsib_direction,
  "Market Double CAR NC" = m4.2_gsib_directionNC,
  "FF Double CAR" = ff3_r4_gsib_direction,
  "Lagged Double CAR" = lag_m4_2_gsib_direction,
  "Market Triple CAR" = m4.3_triple_gsib,
  "Market Triple CAR NC" = m4.3_triple_gsibNC,
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

#corrmatrix
library(dplyr)
library(tibble)

corr_data <- final_esm_data %>%
  mutate(log_TA = log(TotalAssets)) %>%
  select(CAR, abs_CAR, Regulation, Supervision, Tightness,
         ROA, log_TA, CapProxy) %>%
  mutate(across(everything(), as.numeric))

cor_matrix <- cor(corr_data, use = "pairwise.complete.obs")
cor_matrix <- round(cor_matrix, 2)
cor_matrix_lower <- cor_matrix
cor_matrix_lower[upper.tri(cor_matrix_lower)] <- NA

cor_table <- as.data.frame(cor_matrix_lower)
cor_table <- rownames_to_column(cor_table, var = "")

cor_table[is.na(cor_table)] <- "-"

cor_long <- cor_long %>%
  mutate(
    RowVar = factor(RowVar, levels = rev(rownames(cor_matrix))),
    ColVar = factor(ColVar, levels = colnames(cor_matrix))
  )

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(flextable)
library(officer)

# STEP 1: prepare correlation data
corr_data <- final_esm_data %>%
  mutate(log_TA = log(TotalAssets)) %>%
  select(CAR, abs_CAR, Regulation, Supervision, Tightness,
         ROA, log_TA, CapProxy) %>%
  mutate(across(everything(), as.numeric))

# STEP 2: compute correlation matrix
cor_matrix <- cor(corr_data, use = "pairwise.complete.obs")
cor_matrix <- round(cor_matrix, 2)
sprintf("%.2f", cor_matrix)

# STEP 3: assign clean variable names
clean_names <- c("CAR", "Absolute CAR", "Regulation", "Supervision",
                 "Tightness", "ROA", "Log Total Assets", "Capital Ratio Proxy")

colnames(cor_matrix) <- clean_names
rownames(cor_matrix) <- clean_names

# STEP 4: create lower-triangle version for plot and table
cor_matrix_lower <- cor_matrix
cor_matrix_lower[upper.tri(cor_matrix_lower)] <- NA

# STEP 5: create long format for plotting
cor_long <- as.data.frame(cor_matrix_lower) %>%
  rownames_to_column(var = "RowVar") %>%
  pivot_longer(
    cols = -RowVar,
    names_to = "ColVar",
    values_to = "Correlation"
  ) %>%
  mutate(
    RowVar = factor(RowVar, levels = rev(clean_names)),
    ColVar = factor(ColVar, levels = clean_names)
  )

# STEP 6: plot correlation matrix
ggplot(cor_long, aes(x = ColVar, y = RowVar, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(is.na(Correlation), "-", sprintf("%.2f", Correlation))),
            size = 3) +
  scale_fill_gradient2(midpoint = 0, limits = c(-1, 1), na.value = "white") +
  labs(x = NULL, y = NULL, fill = "Correlation") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

# STEP 7: create table version with "-" instead of NA
cor_table <- as.data.frame(cor_matrix_lower) %>%
  rownames_to_column(var = "Variable")

cor_table[is.na(cor_table)] <- "-"

# STEP 8: create clean flextable
ft_corr <- flextable(cor_table)
ft_corr <- theme_booktabs(ft_corr)
ft_corr <- autofit(ft_corr)
ft_corr <- align(ft_corr, j = 1, align = "left", part = "all")
ft_corr <- align(ft_corr, j = 2:ncol(cor_table), align = "center", part = "all")
ft_corr <- bold(ft_corr, part = "header")
ft_corr <- bold(ft_corr, j = 1, part = "body")
ft_corr <- fontsize(ft_corr, size = 10, part = "all")

ft_corr

# STEP 9: export to Word
doc <- read_docx()
doc <- body_add_par(doc, "Table X: Correlation matrix for key variables", style = "Normal")
doc <- body_add_par(
  doc,
  "This matrix reports pairwise Pearson correlation coefficients among the main analytical variables. Correlations are generally moderate, indicating no evidence of problematic multicollinearity.",
  style = "Normal"
)
doc <- body_add_flextable(doc, ft_corr)

print(doc, target = "correlation_matrix.docx")
# time series rob
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)

# STEP 1: prepare monthly time-series data
ts_data <- final_esm_data %>%
  mutate(
    year_month = as.Date(paste0(year_month, "-01"))
  ) %>%
  group_by(year_month) %>%
  summarise(
    avg_CAR = mean(CAR, na.rm = TRUE),
    avg_comm_intensity = mean((Regulation + Supervision) / 2, na.rm = TRUE),
    crisis_month = max(crisis, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year_month)

# STEP 2: compute 6-month moving averages and standardise
ts_data <- ts_data %>%
  mutate(
    avg_CAR_ma6 = zoo::rollmean(avg_CAR, k = 6, fill = NA, align = "right"),
    avg_comm_ma6 = zoo::rollmean(avg_comm_intensity, k = 6, fill = NA, align = "right"),
    CAR_index = as.numeric(scale(avg_CAR_ma6)),
    Comm_index = as.numeric(scale(avg_comm_ma6))
  )

# STEP 3: create crisis groups for shading
ts_data <- ts_data %>%
  mutate(
    crisis_change = crisis_month != lag(crisis_month, default = first(crisis_month)),
    crisis_group = cumsum(crisis_change)
  )

# STEP 4: create crisis shading periods
crisis_periods <- ts_data %>%
  filter(crisis_month == 1) %>%
  group_by(crisis_group) %>%
  summarise(
    xmin = min(year_month),
    xmax = max(year_month),
    duration = as.numeric(difftime(max(year_month), min(year_month), units = "days")) / 30,
    .groups = "drop"
  ) %>%
  filter(duration >= 3)

# STEP 5: reshape for plotting
ts_plot_data <- ts_data %>%
  select(year_month, CAR_index, Comm_index) %>%
  pivot_longer(
    cols = c(CAR_index, Comm_index),
    names_to = "Series",
    values_to = "Value"
  ) %>%
  mutate(
    Series = recode(
      Series,
      "CAR_index" = "Average abnormal returns",
      "Comm_index" = "Communication intensity"
    )
  )

# STEP 6: plot
ggplot() +
  geom_rect(
    data = crisis_periods,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = -Inf,
      ymax = Inf,
      fill = "Crisis period"
    ),
    inherit.aes = FALSE,
    alpha = 0.15
  ) +
  geom_line(
    data = ts_plot_data %>% filter(Series == "Average abnormal returns"),
    aes(x = year_month, y = Value, color = "Average abnormal returns"),
    linewidth = 1.1,
    na.rm = TRUE
  ) +
  geom_line(
    data = ts_plot_data %>% filter(Series == "Communication intensity"),
    aes(x = year_month, y = Value, color = "Communication intensity"),
    linewidth = 1.1,
    na.rm = TRUE
  ) +
  scale_color_manual(
    values = c(
      "Average abnormal returns" = "steelblue",
      "Communication intensity" = "firebrick"
    )
  ) +
  scale_fill_manual(
    values = c("Crisis period" = "grey70")
  ) +
  labs(
    x = NULL,
    y = "Standardised 6-month moving average",
    color = NULL,
    fill = NULL,
    title = "Evolution of average abnormal returns and communication intensity over time"
  ) +
  scale_x_date(
    date_labels = "%Y",
    date_breaks = "2 years"
  ) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 2)
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

library(car)
# Functie om VIF te berekenen via een hulp-OLS (omdat vif() niet op feols werkt)
check_vif <- function(model_formula, data_set) {
  # We halen de onafhankelijke variabelen uit jouw feols formule
  temp_ols <- lm(model_formula, data = data_set)
  return(vif(temp_ols))
}

# --- ONDERZOEKSVRAAG 1 & 2 (Basis) ---
# Check de VIF op de variabelen die in m1.1, m1.2 en m2.1 zitten
vif_ov1_2 <- check_vif(CAR ~ Regulation + Supervision + Tightness + ROA + log(TotalAssets) + CapProxy, 
                       final_esm_data)

# --- ONDERZOEKSVRAAG 3 (Power) ---
# Check de hoofdeffecten voor m3.1 en m3.2
vif_ov3 <- check_vif(CAR ~ Regulation + Supervision + Tightness + Has_Supervisory_Power + ROA + log(TotalAssets) + CapProxy, 
                     final_esm_data)

# --- ONDERZOEKSVRAAG 4 (G-SIB) ---
# Check de hoofdeffecten voor m4.1 en m4.2
vif_ov4 <- check_vif(CAR ~ Regulation + Supervision + Tightness + is_GSIB + ROA + log(TotalAssets) + CapProxy, 
                     final_esm_data)

# --- ONDERZOEKSVRAAG 5 (Crisis & VIX) ---
# Voor m5.1 t/m m5.3 (Crisis)
vif_ov5_crisis <- check_vif(CAR ~ Regulation + Supervision + Tightness + crisis + ROA + log(TotalAssets) + CapProxy, 
                            final_esm_data)

# Voor m5.4 t/m m5.6 (VIX)
vif_ov5_vix <- check_vif(CAR ~ Regulation + Supervision + Tightness + VIX_Level + ROA + log(TotalAssets) + CapProxy, 
                         final_esm_data)

# --- RESULTATEN PRINTEN ---
vif_ov1_2
vif_ov3
vif_ov4
vif_ov5_crisis
vif_ov5_vix
