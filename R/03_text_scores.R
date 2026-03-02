#de functie assign_trichotomous_score definiëren omdat we dit nodig zullen hebben om in groepen te verdelen
assign_trichotomous_score <- function(raw_vector) {
  final_scores <- rep(0, length(raw_vector)) 
  positive_indices <- which(raw_vector > 0)
  positive_values  <- raw_vector[positive_indices]
  
  if(length(positive_values) > 0) {
    bins <- ntile(positive_values, 3)
    final_scores[positive_indices] <- bins - 2
  }
  return(final_scores)
}
#Definieer de verfijnde trefwoordenlijsten
# Voor Tightness gebruiken we de Hawkish (+) en Dovish (-) verdeling
hawkish_terms <- "\\b(increase|raise|higher|tightening|hawkish|restrictive|upside risk|tapering|hike|above target)\\b"
dovish_terms  <- "\\b(decrease|lower|cut|easing|dovish|accommodative|stimulus|downside risk|supportive|below target)\\b"
#Voor Regulation en Supervision blijven we bij de 'intensiteit' trefwoorden
reg_terms <- "\\b(basel|solvency|regulatory framework|capital requirement|leverage ratio|risk-weighting|own funds|capital buffer|liquidity ratio|lcr|nsfr|liquidity coverage|macroprudential|systemic risk|crr|crd)\\b"
sup_terms <- "\\b(oversight|monitoring|inspection|examination|enforcement|sanctions|remedial action|early intervention|stress test|srep|audit|supervisory review|on-site|reporting obligations|disclosure|compliance)\\b"
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

#Voor Regulation en Supervision behouden we de -1, 0, 1 intervallen (robuustheid)
# (Gebruik de 'assign_trichotomous_score' functie die je al in je script had staan)
speeches_subset <- speeches_subset %>%
  mutate(
    Regulation  = assign_trichotomous_score(raw_R),
    Supervision = assign_trichotomous_score(raw_S)
  )
#Controleer het resultaat
print("Samenvatting Tightness (Z-score):")
summary(speeches_subset$Tightness)
print("Verdeling Regulation & Supervision:")
table(speeches_subset$Regulation)
table(speeches_subset$Supervision)










