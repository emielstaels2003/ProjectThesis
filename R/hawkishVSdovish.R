# ============================================================
# HAWKISH VS DOVISH: CONTRASTIVE DICTIONARY EXTRACTION
# Paste this AFTER you have created:
#   - speeches_subset
#   - tokens_clean
#   - stem_lookup
#   - custom_stopwords
#
# This block:
#   1. defines sharper hawkish/dovish seeds
#   2. counts seed hits per speech
#   3. keeps only clearly hawkish vs clearly dovish speeches
#   4. extracts distinctive STEMS using tf-idf
#   5. extracts distinctive BIGRAMS using tf-idf
#   6. exports candidate files
# ============================================================

# ---------------------------
# 1. SHARPER TONE SEEDS
# ---------------------------
seed_hawkish_tone <- c(
  "tightening", "restrictive", "hike", "hikes", "higher",
  "inflationary", "persistent", "vigilance", "overheating", "anchoring"
)

seed_dovish_tone <- c(
  "easing", "accommodative", "stimulus", "cut", "cuts",
  "lower", "slowdown", "weakness", "slack"
)

hawkish_stems <- unique(SnowballC::wordStem(seed_hawkish_tone, language = "en"))
dovish_stems  <- unique(SnowballC::wordStem(seed_dovish_tone, language = "en"))

# ---------------------------
# 2. COUNT HAWKISH / DOVISH HITS PER DOCUMENT
# ---------------------------
tone_doc_hits <- tokens_clean %>%
  dplyr::group_by(doc_id) %>%
  dplyr::summarise(
    hawkish_hits = sum(stem %in% hawkish_stems, na.rm = TRUE),
    dovish_hits  = sum(stem %in% dovish_stems, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    net_tone = hawkish_hits - dovish_hits
  )

# quick overview
print(summary(tone_doc_hits$hawkish_hits))
print(summary(tone_doc_hits$dovish_hits))
print(summary(tone_doc_hits$net_tone))

# ---------------------------
# 3. KEEP ONLY CLEAR CONTRAST CASES
# stricter separation:
#   hawkish speeches: at least 2 hawkish hits and 0 dovish hits
#   dovish speeches: at least 2 dovish hits and 0 hawkish hits
# ---------------------------
tone_contrast_docs <- tone_doc_hits %>%
  dplyr::mutate(
    tone_group = dplyr::case_when(
      hawkish_hits >= 2 & dovish_hits == 0 ~ "hawkish",
      dovish_hits >= 2 & hawkish_hits == 0 ~ "dovish",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(tone_group))

# inspect balance
print(table(tone_contrast_docs$tone_group))

# save the speech-level tone contrast labels
readr::write_csv(tone_contrast_docs, "tone_contrast_docs.csv")

# ---------------------------
# 4. TF-IDF ON STEMS: HAWKISH VS DOVISH
# ---------------------------
tone_tfidf_stems <- tokens_clean %>%
  dplyr::inner_join(
    tone_contrast_docs %>% dplyr::select(doc_id, tone_group),
    by = "doc_id"
  ) %>%
  dplyr::count(tone_group, stem, sort = TRUE) %>%
  tidytext::bind_tf_idf(term = stem, document = tone_group, n = n) %>%
  dplyr::left_join(stem_lookup, by = "stem") %>%
  dplyr::left_join(
    tokens_clean %>%
      dplyr::distinct(doc_id, stem) %>%
      dplyr::count(stem, name = "doc_freq"),
    by = "stem"
  ) %>%
  dplyr::filter(doc_freq >= 10)

# remove vague shared macro words
tone_noise_stems <- c(
  "economi", "econom", "growth", "market", "price", "financi",
  "public", "global", "stabil", "develop", "condit", "outlook",
  "polici", "monetari"
)

tone_tfidf_stems_clean <- tone_tfidf_stems %>%
  dplyr::filter(!stem %in% tone_noise_stems) %>%
  dplyr::arrange(tone_group, dplyr::desc(tf_idf))

hawkish_candidates_stems <- tone_tfidf_stems_clean %>%
  dplyr::filter(tone_group == "hawkish") %>%
  dplyr::arrange(dplyr::desc(tf_idf))

dovish_candidates_stems <- tone_tfidf_stems_clean %>%
  dplyr::filter(tone_group == "dovish") %>%
  dplyr::arrange(dplyr::desc(tf_idf))

# preview top stem candidates
cat("\nTOP HAWKISH STEM CANDIDATES\n")
print(hawkish_candidates_stems %>% dplyr::select(example_word, stem, tf_idf, doc_freq) %>% head(30))

cat("\nTOP DOVISH STEM CANDIDATES\n")
print(dovish_candidates_stems %>% dplyr::select(example_word, stem, tf_idf, doc_freq) %>% head(30))

# export stem candidates
readr::write_csv(hawkish_candidates_stems, "hawkish_candidates_stems.csv")
readr::write_csv(dovish_candidates_stems, "dovish_candidates_stems.csv")

# ---------------------------
# 5. BIGRAMS FOR BETTER TONE SEPARATION
# ---------------------------
# 5. BIGRAMS FOR BETTER TONE SEPARATION (FASTER VERSION)
# only use clearly hawkish / clearly dovish speeches
# ---------------------------

# keep only speeches in the tone contrast sample
tone_contrast_texts <- speeches_subset %>%
  dplyr::inner_join(
    tone_contrast_docs %>% dplyr::select(doc_id, tone_group),
    by = "doc_id"
  ) %>%
  dplyr::select(doc_id, tone_group, text)

# create bigrams only on this reduced sample
bigrams_clean <- tone_contrast_texts %>%
  tidytext::unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  tidyr::separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  dplyr::filter(
    stringr::str_detect(word1, "^[a-z]+$"),
    stringr::str_detect(word2, "^[a-z]+$")
  ) %>%
  dplyr::filter(
    !word1 %in% custom_stopwords$word,
    !word2 %in% custom_stopwords$word
  ) %>%
  tidyr::unite(bigram, word1, word2, sep = " ")

# optional extra filter: drop very rare bigrams before tf-idf
bigram_counts <- bigrams_clean %>%
  dplyr::count(tone_group, bigram, sort = TRUE)

tone_tfidf_bigrams <- bigram_counts %>%
  tidytext::bind_tf_idf(term = bigram, document = tone_group, n = n) %>%
  dplyr::filter(n >= 5) %>%
  dplyr::arrange(tone_group, dplyr::desc(tf_idf))

hawkish_candidates_bigrams <- tone_tfidf_bigrams %>%
  dplyr::filter(tone_group == "hawkish") %>%
  dplyr::arrange(dplyr::desc(tf_idf))

dovish_candidates_bigrams <- tone_tfidf_bigrams %>%
  dplyr::filter(tone_group == "dovish") %>%
  dplyr::arrange(dplyr::desc(tf_idf))

cat("\nTOP HAWKISH BIGRAM CANDIDATES\n")
print(hawkish_candidates_bigrams %>% dplyr::select(bigram, tf_idf, n) %>% head(30))

cat("\nTOP DOVISH BIGRAM CANDIDATES\n")
print(dovish_candidates_bigrams %>% dplyr::select(bigram, tf_idf, n) %>% head(30))

readr::write_csv(hawkish_candidates_bigrams, "hawkish_candidates_bigrams.csv")
readr::write_csv(dovish_candidates_bigrams, "dovish_candidates_bigrams.csv")

# ---------------------------
# 6. PRELIMINARY FINAL TONE DICTIONARIES
# top 20 STEMS + top 20 BIGRAMS as first-pass candidates
# you should manually review these
# ---------------------------
prelim_hawkish_dictionary <- list(
  stems = hawkish_candidates_stems %>%
    dplyr::select(example_word, stem, tf_idf, doc_freq) %>%
    dplyr::distinct(stem, .keep_all = TRUE) %>%
    dplyr::slice_head(n = 20),
  bigrams = hawkish_candidates_bigrams %>%
    dplyr::select(bigram, tf_idf, n) %>%
    dplyr::slice_head(n = 20)
)

prelim_dovish_dictionary <- list(
  stems = dovish_candidates_stems %>%
    dplyr::select(example_word, stem, tf_idf, doc_freq) %>%
    dplyr::distinct(stem, .keep_all = TRUE) %>%
    dplyr::slice_head(n = 20),
  bigrams = dovish_candidates_bigrams %>%
    dplyr::select(bigram, tf_idf, n) %>%
    dplyr::slice_head(n = 20)
)

cat("\n==============================\n")
cat("PRELIMINARY HAWKISH DICTIONARY\n")
cat("==============================\n")
print(prelim_hawkish_dictionary$stems)
print(prelim_hawkish_dictionary$bigrams)

cat("\n==============================\n")
cat("PRELIMINARY DOVISH DICTIONARY\n")
cat("==============================\n")
print(prelim_dovish_dictionary$stems)
print(prelim_dovish_dictionary$bigrams)

# optional exports
readr::write_csv(prelim_hawkish_dictionary$stems, "prelim_hawkish_dictionary_stems.csv")
readr::write_csv(prelim_hawkish_dictionary$bigrams, "prelim_hawkish_dictionary_bigrams.csv")
readr::write_csv(prelim_dovish_dictionary$stems, "prelim_dovish_dictionary_stems.csv")
readr::write_csv(prelim_dovish_dictionary$bigrams, "prelim_dovish_dictionary_bigrams.csv")