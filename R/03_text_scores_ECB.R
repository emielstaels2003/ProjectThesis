# Definieer de verfijnde trefwoordenlijsten
# Voor Tightness gebruiken we de Hawkish (+) en Dovish (-) verdeling
hawkish_terms <- "\\b(restrictions|anchored|persistent|tightening|vigilance|hike|overheating|anchor currency|nominal anchor|monetary anchor|anchor inflation|firmly anchored)\\b"
dovish_terms  <- "\\b(lower|weak|easing|cut|accommodative|slowdown|stimulus|slack|qqe|quantitative and qualitative easing|monetary easing|quantitative easing|lower bound|easing policy|accommodative monetary|spare capacity|monetary stimulus)\\b"

# Voor Regulation en Supervision blijven we bij de 'intensiteit' trefwoorden
reg_terms <- "\\b(capital|basel|requirements|standards|regulation|regulatory|rules|buffer|compliance|lcr|ccyb|tlac|sifis|sibs|ccar|aml|kyc|bcbs|microprudential|Liquidity Coverage Ratio|Countercyclical Capital Buffer|Total Loss-Absorbing Capacity|Systemically Important Financial Institutions|Systemically Important Banks|Comprehensive Capital Analysis and Review|Anti-Money Laundering|Know Your Customer|Basel Committee on Banking Supervision)\\b"
sup_terms <- "\\b(supervision|supervisory|oversight|monitoring|compliance|inspection|surveillance|review|assessment|prudential)\\b"

# Bereken de scores
speeches_robustness <- speeches_robustness %>%
  mutate(
    # Voorbereiding: tekst naar kleine letters en woordenaantal
    text_low = tolower(text),
    text_up  = toupper(text),
    word_count = str_count(text, "\\w+"),
    
    # A. TIGHTNESS: IMF Sentiment Formule (Netto Sentiment)
    pos_count = str_count(text_low, hawkish_terms),
    neg_count = str_count(text_low, dovish_terms),
    raw_T_sentiment = (pos_count - neg_count) / word_count,
    
    # B. REGULATION & SUPERVISION: Ruwe intensiteit
    raw_R = str_count(text_low, reg_terms) / word_count,
    raw_S = str_count(text_low, sup_terms) / word_count
  )

# Omzetten naar finale variabelen
# Voor Tightness gebruiken we de continue Z-score
speeches_robustness <- speeches_robustness %>%
  mutate(
    Tightness = (raw_T_sentiment - mean(raw_T_sentiment, na.rm=TRUE)) / sd(raw_T_sentiment, na.rm=TRUE)
  )

# Continue variabelen gebruiken via Z-score
speeches_robustness <- speeches_robustness %>%
  mutate(
    # Regulation: continu via Z-score
    Regulation = (raw_R - mean(raw_R, na.rm=TRUE)) / sd(raw_R, na.rm=TRUE),
    
    # Supervision: continu via Z-score
    Supervision = (raw_S - mean(raw_S, na.rm=TRUE)) / sd(raw_S, na.rm=TRUE)
  )
