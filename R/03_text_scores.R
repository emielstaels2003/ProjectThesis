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

# De 6 ontkenningswoorden van L&M
negations <- c("NO", "NOT", "NONE", "NEITHER", "NEVER", "NOBODY")
negation_pattern <- paste0("\\b(", paste(negations, collapse = "|"), ")\\b")

# Functie om positieve woorden te tellen met correctie voor negatie
count_pos_with_negation <- function(text_vector, pos_words_list) {
  sapply(text_vector, function(txt) {
    # Splits tekst in losse woorden
    words <- str_split(txt, "\\s+")[[1]]
    if(length(words) == 0) return(0)
    
    # Zoek welke posities in de tekst een positief woord bevatten
    pos_indices <- which(words %in% pos_words_list)
    
    if(length(pos_indices) == 0) return(0)
    
    valid_pos_count <- 0
    
    for (idx in pos_indices) {
      # Kijk naar de 3 woorden vóór het positieve woord (bereik van L&M)
      start_range <- max(1, idx - 3)
      end_range <- max(1, idx - 1)
      
      prev_words <- words[start_range:end_range]
      
      # Controleer of er een ontkenning in dit bereik zit
      has_negation <- any(prev_words %in% negations)
      
      # Alleen tellen als er GEEN ontkenning is
      if (!has_negation) {
        valid_pos_count <- valid_pos_count + 1
      }
    }
    return(valid_pos_count)
  })
}

#Definieer de verfijnde trefwoordenlijsten
# Voor Tightness gebruiken we de Hawkish (+) en Dovish (-) verdeling
hawkish_terms <- "\\b(restrictions|anchored|persistent|tightening|vigilance|hike|overheating|anchor currency|nominal anchor|monetary anchor|anchor inflation|firmly anchored)\\b"
dovish_terms  <- "\\b(lower|weak|easing|cut|accommodative|slowdown|stimulus|slack|qqe|quantitative and qualitative easing|monetary easing|quantitative easing|lower bound|easing policy|accommodative monetary|spare capacity|monetary stimulus)\\b"
#Voor Regulation en Supervision blijven we bij de 'intensiteit' trefwoorden
reg_terms <- "\\b(capital|basel|requirements|standards|regulation|regulatory|rules|buffer|compliance|lcr|ccyb|tlac|sifis|sibs|ccar|aml|kyc|bcbs|microprudential|Liquidity Coverage Ratio|Countercyclical Capital Buffer|Total Loss-Absorbing Capacity|Systemically Important Financial Institutions|Systemically Important Banks|Comprehensive Capital Analysis and Review|Anti-Money Laundering|Know Your Customer|Basel Committee on Banking Supervision)\\b"
sup_terms <- "\\b(supervision|supervisory|oversight|monitoring|compliance|inspection|surveillance|review|assessment|prudential)\\b"
#Voor Sentiment
neg_pattern <- paste0("\\b(", paste(negatieve_woorden, collapse = "|"), ")\\b")
pos_pattern <- paste0("\\b(", paste(positieve_woorden, collapse = "|"), ")\\b")
#Bereken de scores
speeches_subset <- speeches_subset %>%
  mutate(
    # Voorbereiding: tekst naar kleine letters en woordenaantal
    text_low = tolower(text),
    text_up  = toupper(text),
    word_count = str_count(text, "\\w+"),
    
    # A. TIGHTNESS: IMF Sentiment Formule (Netto Sentiment)
    pos_count = str_count(text_low, hawkish_terms),
    neg_count = str_count(text_low, dovish_terms),
    raw_T_sentiment = (pos_count - neg_count) / word_count,
    
    # B. REGULATION & SUPERVISION: Ruwe intensiteit (zoals voorheen)
    raw_R = str_count(text_low, reg_terms) / word_count,
    raw_S = str_count(text_low, sup_terms) / word_count,
    
    # C. SENTIMENT CONTROLE (L&M Methode)
    lm_neg_count = str_count(text_up, neg_pattern),
    lm_pos_count = count_pos_with_negation(text_up, positieve_woorden),
    raw_sentiment = (lm_pos_count - lm_neg_count) / word_count
  )
#Omzetten naar finale variabelen
# Voor Tightness gebruiken we de continue Z-score (nauwkeuriger voor regressie)
speeches_subset <- speeches_subset %>%
  mutate(
    Tightness = (raw_T_sentiment - mean(raw_T_sentiment, na.rm=TRUE)) / sd(raw_T_sentiment, na.rm=TRUE),
    Sentiment = (raw_sentiment - mean(raw_sentiment, na.rm=TRUE)) / sd(raw_sentiment, na.rm=TRUE),
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
print("Samenvatting Sentiment (Z-score):")
summary(speeches_subset$Sentiment)
print("Verdeling Regulation & Supervision:")
table(speeches_subset$Regulation)
table(speeches_subset$Supervision)
View(speeches_subset)








