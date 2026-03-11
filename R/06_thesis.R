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
