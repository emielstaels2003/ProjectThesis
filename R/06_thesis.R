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
