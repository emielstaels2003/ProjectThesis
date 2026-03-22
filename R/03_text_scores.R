#zero inflated intensity for reg and sup bin making

assign_zero_inflated_intensity <- function(x) {
  
  scores <- rep(0, length(x))   # default = 0 (no mention)
  
  positive_idx <- which(x > 0)
  
  if(length(positive_idx) > 0) {
    
    positive_values <- x[positive_idx]
    
    # split positive speeches at median
    threshold <- median(positive_values, na.rm = TRUE)
    
    scores[positive_idx] <- ifelse(positive_values <= threshold, 1, 2)
  }
  
  return(scores)
}

#Definieer de verfijnde trefwoordenlijsten
# Voor Tightness gebruiken we de Hawkish (+) en Dovish (-) verdeling
hawkish_terms <- "\\b(restrict|tighten|overheat|inflationary pressures|quantitative tightening|anchoring inflation|vigilance)\\b"
dovish_terms  <- "\\b(cut|accommodative|slowdown|stimulus|slack|monetary easing|quantitative easing|easing|weakness)\\b"
#Voor Regulation en Supervision blijven we bij de 'intensiteit' trefwoorden
reg_terms <- "\\b(capital|basel|requirements|standards|regulation|regulatory|rules|buffer|compliance|lcr|ccyb|tlac|sifis|sibs|ccar|aml|kyc|bcbs|microprudential)\\b"
sup_terms <- "\\b(supervision|supervisory|oversight|monitoring|compliance|inspection|surveillance|review|assessment|prudential)\\b"
#Bereken de scores
speeches_subset <- speeches_subset %>%
  mutate(
    # Voorbereiding: tekst naar kleine letters en woordenaantal
    text_low = tolower(text),
    word_count = str_count(text, "\\w+"),
    
    # A. TIGHTNESS: IMF Sentiment Formule (Netto Sentiment)
    pos_count = str_count(text_low, hawkish_terms),
    neg_count = str_count(text_low, dovish_terms),
    # De 0.0001 voorkomt een 'division by zero'
    raw_T_sentiment = (pos_count - neg_count) / (pos_count + neg_count + 0.0001),
    
    # B. REGULATION & SUPERVISION: Ruwe intensiteit (zoals voorheen)
    raw_R = str_count(text_low, reg_terms) / word_count,
    raw_S = str_count(text_low, sup_terms) / word_count
  )
#Omzetten naar finale variabelen
# Voor Tightness gebruiken we de continue Z-score (nauwkeuriger voor regressie)
speeches_subset <- speeches_subset %>%
  mutate(
    Tightness = (raw_T_sentiment - mean(raw_T_sentiment, na.rm=TRUE)) / sd(raw_T_sentiment, na.rm=TRUE)
  )

# (Gebruik de 'assign_zero_inflated_intensity' functie die je al in je script had staan)
speeches_subset <- speeches_subset %>%
  mutate(
    Regulation  = assign_zero_inflated_intensity(raw_R),
    Supervision = assign_zero_inflated_intensity(raw_S)
  )
#Controleer het resultaat
print("Samenvatting Tightness (Z-score):")
summary(speeches_subset$Tightness)
print("Verdeling Regulation & Supervision:")
table(speeches_subset$Regulation)
table(speeches_subset$Supervision)

View(speeches_subset)








