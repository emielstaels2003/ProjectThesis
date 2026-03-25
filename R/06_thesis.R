##### A: APPENDIX BANKEN PER CENTRALE BANK OVERZICHT
# 1. SAMENATTENDE TABEL MAKEN
speech_counts <- speeches_subset %>%
  group_by(CentralBank) %>%
  summarise(Speech_Count = n(), .groups = 'drop')

# 2. MAPPING: Groepeer de banken en koppel de counts
appendix_table_data <- Bank_Mapping %>%
  # Verwijder dubbele rijen (bijv. dezelfde bank in verschillende jaren)
  distinct(CentralBank, Bank_Name) %>%
  # Groepeer per centrale bank om de lijst met banken te maken
  group_by(CentralBank) %>%
  summarise(
    Total_Banks = n(),
    # Maak een mooie lijst van banken gescheiden door een komma
    Bank_List = paste(sort(Bank_Name), collapse = ", "),
    .groups = 'drop'
  ) %>%
  # Koppel het aantal speeches aan de tabel
  left_join(speech_counts, by = "CentralBank") %>%
  # Als een bank 0 speeches heeft, zet dan 0 neer i.p.v. NA
  mutate(Speech_Count = coalesce(as.numeric(Speech_Count), 0)) %>%
  # Zet de kolommen in de gevraagde volgorde: Speeches links
  select(Speech_Count, CentralBank, Total_Banks, Bank_List) %>%
  # Sorteer alfabetisch op Centrale Bank
  arrange(CentralBank)

# 3. OPMAAK: Maak de professionele tabel met gt
final_table <- appendix_table_data %>%
  gt() %>%
  # Geef de kolommen nette namen
  cols_label(
    Speech_Count = "Speeches",
    CentralBank = "Central Bank",
    Total_Banks = "Number of Banks",
    Bank_List = "Included Institutions"
  ) %>%
  # VOEG DIKTE TOE: Pas de padding aan (boven/onder witruimte)
  tab_options(
    data_row.padding = px(15),        # Maakt de rijen dikker
    table.width = pct(100),           # Gebruik de volledige breedte van de pagina
    column_labels.font.weight = "bold",
    column_labels.background.color = "#F9F9F9"
  ) %>%
  # Zorg dat de tekst in de lijst van banken mooi doorloopt (wrapping)
  cols_width(
    Speech_Count ~ px(100),
    CentralBank ~ px(150),
    Total_Banks ~ px(120),
    Bank_List ~ px(460)
  ) %>%
  # Tekst uitlijning verbeteren
  cols_align(
    align = "center",
    columns = c(Speech_Count, Total_Banks)
  )
# Toon het resultaat
final_table


# AANVULLEND OP VORIGE CODE: TABEL EXPORTEREN NAAR EEN WORD DOCUMENT OM IN THESIS TE ZETTEN
# 1. Installeer en laad de benodigde extra bibliotheken
if(!require(flextable)) install.packages("flextable")
if(!require(officer)) install.packages("officer")
if(!require(webshot2)) install.packages("webshot2") # Nodig om een foto te maken van gt
library(flextable)
library(officer)

library(webshot2)

# 2. De data omzetten naar een Word-tabel (flextable)
word_export <- flextable(appendix_table_data) %>%
  # Kolomnamen netjes maken
  set_header_labels(
    Speech_Count = "Speeches",
    CentralBank = "Central Bank",
    Total_Banks = "Number of Banks",
    Bank_List = "Included Institutions"
  ) %>%
  # Opmaak: Dikke koppen en grijze achtergrond (zoals in je gt-code)
  bold(part = "header") %>%
  bg(bg = "#F9F9F9", part = "header") %>%
  # Rijen dikker maken (padding)
  padding(padding = 10, part = "all") %>%
  # Zorg dat de tekst in de kolommen mooi doorloopt (wrapping)
  # We stellen de breedte in inches in (totaal ongeveer 6.5 tot 7.0 voor A4)
  width(j = "Speech_Count", width = 0.8) %>%
  width(j = "CentralBank", width = 1.2) %>%
  width(j = "Total_Banks", width = 1.0) %>%
  width(j = "Bank_List", width = 4.0) %>%
  # Tekst uitlijnen (midden voor de cijfers)
  align(j = c("Speech_Count", "Total_Banks"), align = "center", part = "all") %>%
  # Standaard font voor scripties
  font(fontname = "Times New Roman", part = "all")

# 3. OPSLAAN IN JE WERKMAPP
# Dit bestand verschijnt direct in je 'Files' paneel rechtsonder in RStudio
save_as_docx(word_export, path = "Appendix_Tabel_Banken.docx")
# Bevestiging in je console
print("KLAAR! Het bestand 'Appendix_Tabel_Banken.docx' staat nu in je werkmap.")




##### B: GRAFIEK VAN AANTAL SPEECHES PER JAAR VOOR ALLE TARGET BANKS
# grafiek met aantal speeches per centrale bank
library(dplyr)
library(ggplot2)
library(lubridate)

# 1. Voorbereiding van de data
plot_data <- speeches_subset %>%
  # Filter op de gewenste centrale banken
  filter(CentralBank %in% target_banks) %>%
  # Zorg dat de datum goed herkend wordt en extraheer het jaar
  mutate(Year = year(as.Date(Date_Original))) %>%
  # Filter op de gevraagde periode
  filter(Year >= 1997 & Year <= 2023) %>%
  # Tel het aantal speeches per bank per jaar
  group_by(CentralBank, Year) %>%
  summarise(NumSpeeches = n(), .groups = "drop")

# 2. De grafiek maken
ggplot(plot_data, aes(x = Year, y = NumSpeeches, color = CentralBank, group = CentralBank)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  # Gebruik een schone lay-out
  theme_minimal() +
  labs(
    title = "Aantal Speeches per Centrale Bank (1997 - 2023)",
    x = "Jaar",
    y = "Aantal Speeches",
    color = "Centrale Bank"
  ) +
  # Zorg dat alle jaren op de x-as passen (om de 2 of 5 jaar voor leesbaarheid)
  scale_x_continuous(breaks = seq(1997, 2023, by = 2)) +
  # Verplaats de legende naar onderen omdat het veel banken zijn
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  # Gebruik een kleurenpalet dat geschikt is voor veel lijnen
  scale_color_viridis_d(option = "turbo")




##### C: GRAFIEK VAN AANTAL SPEECHES PER JAAR VOOR TOP 5 TARGET BANKS
# 1. Bepaal de Top 5 banken met de meeste speeches in totaal
top_5_banks <- speeches_subset %>%
  filter(CentralBank %in% target_banks) %>%
  group_by(CentralBank) %>%
  summarise(total = n()) %>%
  arrange(desc(total)) %>%
  slice(1:5) %>%
  pull(CentralBank)
top_5_banks

# 2. Prepare the data: Group all other banks as "Other Central Banks"
plot_data_grouped <- speeches_subset %>%
  filter(CentralBank %in% target_banks) %>%
  mutate(Year = year(as.Date(Date_Original))) %>%
  filter(Year >= 1997 & Year <= 2023) %>%
  mutate(GroupedBank = ifelse(CentralBank %in% top_5_banks, CentralBank, "Other Central Banks")) %>%
  group_by(GroupedBank, Year) %>%
  summarise(NumSpeeches = n(), .groups = "drop")

# 3. Create the visualization with professional English labeling
ggplot(plot_data_grouped, aes(x = Year, y = NumSpeeches, color = GroupedBank, group = GroupedBank)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  theme_minimal() +
  labs(
    title = "Annual Speech Volume: Top 5 vs. Other Central Banks",
    subtitle = "The Top 5 represents the most active central banks in the dataset (1997-2023)",
    x = "Year",
    y = "Number of Speeches",
    color = "Central Bank"
  ) +
  scale_x_continuous(breaks = seq(1998, 2022, by = 4)) +
  # Professional color palette for clear distinction
  scale_color_manual(values = c(
    "European Central Bank" = "#003399",                       # ECB Blue
    "Board of Governors of the Federal Reserve" = "#004a2c",    # Fed Green
    "Bank of England" = "#e31b23",                             # BoE Red
    "Reserve Bank of India" = "#ff9933",                       # India Orange
    "Bank of Japan" = "#bc002d",                               # Japan Crimson
    "Other Central Banks" = "grey75"                           # Neutral Grey for grouping
  )) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )


##### D: OVERZICHT DATA SPEECHES_SUBSET
# Install these if you haven't already:
install.packages("gtExtras")
library(gt)
library(gtExtras)
library(dplyr)
library(tidyr)
library(purrr)

# 1. Data voorbereiden: bereken de fracties voor alle 4 de rollen
table_data_complete <- speeches_subset %>%
  filter(CentralBank %in% target_banks) %>%
  group_by(CentralBank) %>%
  summarise(
    Speeches = n(),
    Avg_Words = round(mean(word_count, na.rm = TRUE), 0),
    # Gender (2 labels - dit blijft bar_stack)
    Gender_Data = list(c(sum(Gender == "Female", na.rm = TRUE)/n(), 
                         sum(Gender == "Male", na.rm = TRUE)/n())),
    # Role (4 aparte kolommen voor de visualisatie)
    Gov_Pct = (sum(Role == "Governor", na.rm = TRUE) / n()) * 100,
    Dep_Pct = (sum(Role == "Deputy Governor", na.rm = TRUE) / n()) * 100,
    Board_Pct = (sum(Role == "Board member", na.rm = TRUE) / n()) * 100,
    Senior_Pct = (sum(Role == "Senior management", na.rm = TRUE) / n()) * 100
  ) %>%
  arrange(desc(Speeches))

# 2. De Tabel renderen met aparte visuele indicatoren
final_table_complete <- table_data_complete %>%
  gt() %>%
  # Gender blijft de mooie roze/blauwe balk
  gt_plt_bar_stack(
    column = Gender_Data,
    position = "fill",
    labels = c("Female", "Male"),
    palette = c("#ff4d94", "#3399ff"),
    width = 65,
    fmt_fn = scales::label_percent(accuracy = 1)
  ) %>%
  # Voor de 4 rollen gebruiken we kleur-intensiteit en percentages
  # Dit is de meest overzichtelijke manier voor 4 categorieën
  gt_color_rows(Gov_Pct:Senior_Pct, palette = "Greens", domain = c(0, 100)) %>%
  
  # Opmaak en titels
  tab_header(
    title = md("**Institutional Composition of Central Bank Communication**"),
    subtitle = md("Full Distribution of Gender and Professional Roles (1997-2023). <br>
                  **Gender Legend:** <span style='color:#ff4d94;'>■</span> Female | <span style='color:#3399ff;'>■</span> Male")
  ) %>%
  cols_label(
    Avg_Words = "Avg. Words",
    Gender_Data = "Gender (%)",
    Gov_Pct = "Governor (%)",
    Dep_Pct = "Deputy (%)",
    Board_Pct = "Board (%)",
    Senior_Pct = "Senior Mgmt (%)"
  ) %>%
  fmt_number(columns = ends_with("_Pct"), decimals = 1) %>%
  gt_theme_538() %>%
  tab_options(table.width = pct(100))

# Resultaat
final_table_complete


##### E: CORRELATIEMATRIX1

install.packages("ggcorrplot")
library(ggcorrplot)
library(dplyr)

# 1. Selecteer enkel de continue variabelen
corr_data_clean <- final_esm_data %>%
  mutate(log_Assets = log(TotalAssets)) %>%
  select(
    CAR, 
    Tightness, 
    Regulation, 
    Supervision, 
    ROA, 
    log_Assets, 
    TotalEquity, 
    CapProxy
  )

# 2. Bereken de correlatiematrix
# 'use = "pairwise.complete.obs"' zorgt dat we zoveel mogelijk data behouden
corr_matrix_clean <- cor(corr_data_clean, use = "pairwise.complete.obs")

# 3. Genereer de visualisatie
ggcorrplot(corr_matrix_clean, 
           hc.order = TRUE,           # Groepeer variabelen die op elkaar lijken
           type = "lower",            # Toon alleen de onderste driehoek (voorkomt dubbelop)
           lab = TRUE,                # Toon de cijfers in de vakjes
           lab_size = 4, 
           method = "square", 
           colors = c("#E46726", "white", "#6D9ECB"), 
           title = "Correlation Matrix: Continuous Variables",
           ggtheme = theme_minimal())


##### F: CORRELATIEMATRIX2: nutteloos

library(dplyr)
library(knitr)

# 1. Selecteer de continue variabelen
corr_data_numeric <- final_esm_data %>%
  mutate(log_Assets = log(TotalAssets)) %>%
  select(
    CAR, 
    Tightness, 
    Regulation, 
    Supervision, 
    ROA, 
    log_Assets, 
    TotalEquity, 
    CapProxy
  )

# 2. Bereken de matrix en rond af op 3 decimalen voor leesbaarheid
# 'use = "pairwise.complete.obs"' is essentieel bij ontbrekende data
corr_matrix_numeric <- cor(corr_data_numeric, use = "pairwise.complete.obs")
corr_matrix_rounded <- round(corr_matrix_numeric, 3)

# 3. Print als een nette tabel
kable(corr_matrix_rounded, 
      format = "simple", 
      caption = "Table: Correlation Matrix of Continuous Variables")


##### G: CORRELATIEMATRIX3

# 1. De matrix omzetten naar een dataframe voor gt
# We gebruiken de afgeronde matrix die je zojuist hebt gegenereerd
corr_df <- as.data.frame(corr_matrix_rounded) %>%
  mutate(Variable = rownames(.)) %>%
  select(Variable, everything())

# 2. De Tabel renderen
final_corr_table <- corr_df %>%
  gt() %>%
  # Kleur de cellen op basis van waarde (blauw voor positief, rood voor negatief)
  data_color(
    columns = -Variable,
    colors = scales::col_numeric(
      palette = c("#E46726", "white", "#6D9ECB"),
      domain = c(-1, 1)
    )
  ) %>%
  # Stijl aanpassingen
  tab_header(
    title = md("**Correlation Matrix: Continuous Variables**"),
    subtitle = "Pearson correlation coefficients for Speech and Bank-level indicators"
  ) %>%
  cols_label(
    log_Assets = "log(Assets)",
    TotalEquity = "Equity",
    CapProxy = "Capital"
  ) %>%
  fmt_number(columns = -Variable, decimals = 3) %>%
  gt_theme_538() %>%
  tab_options(
    column_labels.font.weight = "bold",
    table.width = pct(90)
  )

# 3. Bekijken en Exporteren
final_corr_table



##### H: SCORE SUP/REG PER JAAR PER CENTRALE BANK

install.packages("janitor")
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(janitor) # Voor het opschonen van kolomnamen

# 1. Namen opschonen om spaties en hoofdletter-ellende te voorkomen
speeches_clean <- speeches_subset %>% 
  clean_names() 

# 2. Check nu even snel de nieuwe namen in je console:
# names(speeches_clean) 
# Waarschijnlijk heten ze nu 'date_clean', 'raw_r' en 'raw_s'

plot_data <- speeches_clean %>%
  filter(central_bank %in% top_5_banks) %>%
  # We gebruiken 'date_clean' (nu met kleine letters door clean_names)
  mutate(year_val = year(as.Date(date_clean))) %>% 
  mutate(
    Regulation = ifelse(raw_r > 0, "Mentioned (>0)", "Not Mentioned (0)"),
    Supervision = ifelse(raw_s > 0, "Mentioned (>0)", "Not Mentioned (0)")
  ) %>%
  pivot_longer(cols = c(Regulation, Supervision), 
               names_to = "Topic", 
               values_to = "Presence")

# 4. De Grafiek genereren
ggplot(plot_data, aes(x = year_val, fill = Presence)) +
  geom_bar(position = "fill") + 
  facet_grid(Topic ~ central_bank) + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1997, 2023, by = 5)) +
  scale_fill_manual(values = c("Mentioned (>0)" = "#2c7bb6", "Not Mentioned (0)" = "#d7191c")) +
  labs(
    title = "Evolution of Policy Discourse (1997-2023)",
    subtitle = "Relative share of speeches mentioning Regulation vs. Supervision",
    x = "Year",
    y = "Percentage of Total Speeches",
    fill = "Speech Content"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9)
  )



