sentiment_data <- read_excel("data/Loughran-McDonald_MasterDictionary_1993-2024.xlsx")

# ==============================================================================
# GENEREREN VAN POSITIEVE EN NEGATIEVE WOORDENLISTEN (Loughran-McDonald)
# ==============================================================================

# 1. Maak de lijst met negatieve woorden
# We kijken in kolom 'Negative'. Als de waarde groter is dan 0 (het jaartal), 
# dan voegen we het woord uit de kolom 'Word' toe aan onze lijst.
negatieve_woorden <- Loughran_McDonald_MasterDictionary_1993_2024$Word[Loughran_McDonald_MasterDictionary_1993_2024$Negative > 0]

# 2. Maak de lijst met positieve woorden
# We doen hetzelfde voor de kolom 'Positive'.
positieve_woorden <- Loughran_McDonald_MasterDictionary_1993_2024$Word[Loughran_McDonald_MasterDictionary_1993_2024$Positive > 0]

# 3. Opschonen (optioneel maar aangeraden)
# Hiermee verwijder je eventuele lege waarden (NA's) uit de lijsten
negatieve_woorden <- na.omit(negatieve_woorden)
positieve_woorden <- na.omit(positieve_woorden)

# 4. Resultaat controleren in de console
cat("Aantal negatieve woorden gevonden:", length(negatieve_woorden), "\n")
cat("Aantal positieve woorden gevonden:", length(positieve_woorden), "\n")

# Toon de eerste 10 woorden van elke lijst ter controle
print("Eerste 10 negatieve woorden:")
print(negatieve_woorden)

print("Eerste 10 positieve woorden:")
print(positieve_woorden)
