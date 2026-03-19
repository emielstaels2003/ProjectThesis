# ============================================================
# BUILD A THESIS-READY DICTIONARY FROM CENTRAL BANK SPEECHES
# Dataset: speeches_subset
# Required column: text
#
# Topics:
#   1) Tightness_Hawkish
#   2) Tightness_Dovish
#   3) Supervision
#   4) Regulation
#
# What this script does:
#   - tokenizes and stems speeches
#   - uses small seed dictionaries
#   - identifies speeches related to each topic
#   - extracts candidate terms using tf-idf
#   - extracts candidate terms using co-occurrence
#   - combines both into candidate dictionaries
#   - exports clean CSV files for manual review
#   - creates a preliminary final dictionary
#
# IMPORTANT:
#   The exported candidate files are what you review and defend
#   in your thesis. The "final dictionary" is the cleaned result.
# ============================================================

# ---------------------------
# 1. PACKAGES
# ---------------------------
required_packages <- c(
  "tidyverse",
  "tidytext",
  "SnowballC",
  "stopwords"
)

to_install <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

# Create a document id
speeches_subset <- speeches_subset %>%
  mutate(doc_id = row_number()) %>%
  filter(!is.na(text))

#---------------------------
# 3. SEED WORDS1
# These are just starting points, not the final dictionary
# ---------------------------
#seed_hawkish <- c(
#  "tightening", "restrictive", "hike", "higher", "inflation",
#  "persistent", "pressure", "vigilance", "anchor", "overheating"
#)

#seed_dovish <- c(
#  "easing", "accommodative", "support", "stimulus", "lower",
#  "cut", "slowdown", "weakness", "recovery", "unemployment"
#)

#seed_supervision <- c(
#  "supervision", "supervisory", "oversight", "monitoring", "prudential",
#  "compliance", "inspection", "assessment", "review", "surveillance"
#)

#seed_regulation <- c(
#  "regulation", "regulatory", "rules", "standards", "framework",
#  "requirements", "capital", "liquidity", "buffer", "solvency"
#)

#table(doc_topics_all$group)
#other target 
#4863  42125 

# ---------------------------
# 3. SEED WORDS2
# These are just starting points, not the final dictionary
# ---------------------------
#seed_hawkish <- c(
#  "tightening", "restrictive", "pressure", "vigilance", "overheating")
#
#seed_dovish <- c(
#  "easing", "support", "stimulus", "slowdown", "weakness", "recovery")
#
#seed_supervision <- c(
#  "supervision", "supervisory", "oversight", "monitoring", "prudential",
#  "compliance", "inspection", "assessment", "surveillance"
#)

#seed_regulation <- c(
#  "banking supervision", "bank supervision", "glass-steagall", "tarp", 
#  "thrift supervision", "dodd-frank", "financial reform", 
#  "commodity futures trading commission", "cftc", 
#  "house financial services committee", "basel", "capital requirement", 
#  "volcker rule", "bank stress test", 
#  "securities and exchange commission", "sec", 
# "deposit insurance", "fdic", "fslic", "ots", "occ", "firrea")

#table(doc_topics_all$group)
#other target 
#16758  30230 

# ---------------------------
# 3. SEED WORDS3
# These are just starting points, not the final dictionary
# ---------------------------
seed_hawkish <- c("tightening", "restrictive", "pressure", "vigilance", "overheating")

seed_dovish <- c("easing", "support", "stimulus", "slowdown", "weakness", "recovery")

seed_supervision <- c("supervision", "supervisory", "oversight", "monitoring", "prudential",
  "compliance", "inspection", "assessment", "surveillance"
)

seed_regulation <- c(
  "banking supervision", "bank supervision", "glass-steagall", "tarp", 
  "thrift supervision", "dodd-frank", "financial reform", 
  "commodity futures trading commission", "cftc", 
  "house financial services committee", "basel", "capital requirement", 
  "volcker rule", "bank stress test", 
  "securities and exchange commission", "sec", 
 "deposit insurance", "fdic", "fslic", "ots", "occ", "firrea"
)

#table(doc_topics_all$group)
#other target 
#22891  24097

# Stem seeds
seed_tbl <- tibble(
  topic = c(
    rep("tightness_hawkish", length(seed_hawkish)),
    rep("tightness_dovish", length(seed_dovish)),
    rep("supervision", length(seed_supervision)),
    rep("regulation", length(seed_regulation))
  ),
  seed_word = c(seed_hawkish, seed_dovish, seed_supervision, seed_regulation)
) %>%
  mutate(seed_stem = wordStem(seed_word, language = "en")) %>%
  distinct(topic, seed_word, seed_stem)

# ---------------------------
# 4. STOPWORDS
# ---------------------------
data("stop_words")

custom_stopwords <- tibble(
  word = unique(c(
    stop_words$word,
    stopwords("en"),
    # central-bank-specific high-frequency words that may be too generic
    "bank", "banks", "banking",
    "central", "european", "euro",
    "mr", "mrs", "ms",
    "also", "would", "could", "may", "must",
    "today", "year", "years", "time"
  ))
)

# ---------------------------
# 5. TOKENIZE AND CLEAN
# ---------------------------
tokens_clean <- speeches_subset %>%
  select(doc_id, text) %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_to_lower(word)) %>%
  filter(str_detect(word, "^[a-z]+$")) %>%
  filter(nchar(word) >= 3) %>%
  anti_join(custom_stopwords, by = "word") %>%
  mutate(stem = wordStem(word, language = "en")) %>%
  filter(!is.na(stem), stem != "")

# Map stem -> most common original word
stem_lookup <- tokens_clean %>%
  count(stem, word, sort = TRUE) %>%
  group_by(stem) %>%
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(example_word = word, example_word_freq = n)

# Token counts per doc
doc_token_counts <- tokens_clean %>%
  count(doc_id, name = "total_tokens")

# ---------------------------
# 6. LABEL DOCUMENTS PER TOPIC
# A document belongs to a topic if it contains >=1 seed stem
# ---------------------------
doc_topic_hits <- tokens_clean %>%
  inner_join(seed_tbl %>% select(topic, seed_stem), by = c("stem" = "seed_stem")) %>%
  count(doc_id, topic, name = "seed_hits")

doc_topics_all <- expand_grid(
  doc_id = unique(tokens_clean$doc_id),
  topic = unique(seed_tbl$topic)
) %>%
  left_join(doc_topic_hits, by = c("doc_id", "topic")) %>%
  mutate(
    seed_hits = replace_na(seed_hits, 0),
    group = if_else(seed_hits >= 2, "target", "other")
  )

# ---------------------------
# 7. TF-IDF CANDIDATE EXTRACTION
# For each topic: which stems are distinctive in target docs?
# ---------------------------
tfidf_candidates <- map_dfr(unique(seed_tbl$topic), function(topic_name) {
  
  topic_labels <- doc_topics_all %>%
    filter(topic == topic_name) %>%
    select(doc_id, group)
  
  counts_topic <- tokens_clean %>%
    inner_join(topic_labels, by = "doc_id") %>%
    count(group, stem, sort = TRUE)
  
  tfidf_topic <- counts_topic %>%
    bind_tf_idf(term = stem, document = group, n = n) %>%
    filter(group == "target") %>%
    left_join(stem_lookup, by = "stem") %>%
    left_join(
      tokens_clean %>% distinct(doc_id, stem) %>% count(stem, name = "doc_freq"),
      by = "stem"
    ) %>%
    mutate(topic = topic_name) %>%
    select(topic, stem, example_word, n, doc_freq, tf, idf, tf_idf) %>%
    arrange(desc(tf_idf))
  
  tfidf_topic
})

# Filter rare stems
tfidf_candidates_filtered <- tfidf_candidates %>%
  filter(doc_freq >= 10)

# ---------------------------
# 8. CO-OCCURRENCE EXPANSION
# Words near seed stems may be useful candidates
# Window-based approximation using adjacent tokens in each doc
# ---------------------------

# Add token position within each document
tokens_positioned <- speeches_subset %>%
  select(doc_id, text) %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_to_lower(word)) %>%
  filter(str_detect(word, "^[a-z]+$")) %>%
  filter(nchar(word) >= 3) %>%
  anti_join(custom_stopwords, by = "word") %>%
  group_by(doc_id) %>%
  mutate(position = row_number()) %>%
  ungroup() %>%
  mutate(stem = wordStem(word, language = "en")) %>%
  filter(!is.na(stem), stem != "")

window_size <- 5

cooccurrence_candidates <- map_dfr(unique(seed_tbl$topic), function(topic_name) {
  
  topic_seed_stems <- seed_tbl %>%
    filter(topic == topic_name) %>%
    pull(seed_stem) %>%
    unique()
  
  seed_positions <- tokens_positioned %>%
    filter(stem %in% topic_seed_stems) %>%
    select(doc_id, seed_position = position)
  
  nearby_terms <- tokens_positioned %>%
    inner_join(seed_positions, by = "doc_id") %>%
    mutate(distance = abs(position - seed_position)) %>%
    filter(distance > 0, distance <= window_size) %>%
    count(stem, word, sort = TRUE) %>%
    group_by(stem) %>%
    summarise(
      cooc_n = sum(n),
      example_word = word[which.max(n)],
      .groups = "drop"
    ) %>%
    left_join(
      tokens_positioned %>% distinct(doc_id, stem) %>% count(stem, name = "doc_freq"),
      by = "stem"
    ) %>%
    mutate(topic = topic_name) %>%
    select(topic, stem, example_word, cooc_n, doc_freq) %>%
    arrange(desc(cooc_n))
  
  nearby_terms
})

cooccurrence_candidates_filtered <- cooccurrence_candidates %>%
  filter(doc_freq >= 10)

# ---------------------------
# 9. COMBINE TF-IDF + CO-OCCURRENCE
# This produces your candidate dictionary
# ---------------------------
candidate_dictionary <- full_join(
  tfidf_candidates_filtered,
  cooccurrence_candidates_filtered,
  by = c("topic", "stem", "example_word", "doc_freq")
) %>%
  mutate(
    tf_idf = replace_na(tf_idf, 0),
    cooc_n = replace_na(cooc_n, 0)
  ) %>%
  group_by(topic) %>%
  mutate(
    tfidf_rank = min_rank(desc(tf_idf)),
    cooc_rank = min_rank(desc(cooc_n)),
    combined_rank = tfidf_rank + cooc_rank
  ) %>%
  ungroup() %>%
  arrange(topic, combined_rank)

# ---------------------------
# 10. REMOVE OBVIOUS NOISE
# You can add/remove words here after first inspection
# ---------------------------
noise_words <- c(
  "said", "say", "well", "much", "many", "good", "bad",
  "economi", "econom", "growth", "market", "price",
  "public", "global", "financi"
)

candidate_dictionary_clean <- candidate_dictionary %>%
  filter(!stem %in% noise_words)

# ---------------------------
# 11. EXPORT CANDIDATE FILES
# THESE are the important outputs for your thesis work
# ---------------------------
write_csv(tfidf_candidates_filtered, "dictionary_candidates_tfidf.csv")
write_csv(cooccurrence_candidates_filtered, "dictionary_candidates_cooccurrence.csv")
write_csv(candidate_dictionary_clean, "dictionary_candidates_combined.csv")

# ---------------------------
# 12. PRELIMINARY FINAL DICTIONARY
# Keep top 10 candidates per topic as a FIRST draft
# You should inspect and revise this manually
# ---------------------------
preliminary_final_dictionary <- candidate_dictionary_clean %>%
  group_by(topic) %>%
  arrange(combined_rank, .by_group = TRUE) %>%
  distinct(stem, .keep_all = TRUE) %>%
  slice_head(n = 20) %>%
  ungroup() %>%
  select(topic, stem, example_word, tf_idf, cooc_n, combined_rank)

write_csv(preliminary_final_dictionary, "dictionary_preliminary_final.csv")

# ---------------------------
# 13. PRINT PRELIMINARY DICTIONARY
# ---------------------------
cat("\n==============================\n")
cat("PRELIMINARY FINAL DICTIONARY\n")
cat("==============================\n\n")

for (topic_name in unique(preliminary_final_dictionary$topic)) {
  cat("\nTOPIC:", topic_name, "\n")
  print(
    preliminary_final_dictionary %>%
      filter(topic == topic_name) %>%
      select(example_word)
  )
}

# ---------------------------
# 14. CREATE A THESIS-READY DICTIONARY OBJECT
# This uses the preliminary dictionary
# After manual revision, replace with your final chosen words
# ---------------------------
final_dictionary_list <- preliminary_final_dictionary %>%
  group_by(topic) %>%
  summarise(words = list(example_word), .groups = "drop")

print(final_dictionary_list)

# ---------------------------
# 15. OPTIONAL: SAVE A SIMPLE TXT VERSION
# ---------------------------
for (topic_name in unique(preliminary_final_dictionary$topic)) {
  words_out <- preliminary_final_dictionary %>%
    filter(topic == topic_name) %>%
    pull(example_word)
  
  writeLines(words_out, paste0("dictionary_", topic_name, ".txt"))
}

cat("\nFiles saved:\n")
cat("- dictionary_candidates_tfidf.csv\n")
cat("- dictionary_candidates_cooccurrence.csv\n")
cat("- dictionary_candidates_combined.csv\n")
cat("- dictionary_preliminary_final.csv\n")
cat("- dictionary_tightness_hawkish.txt\n")
cat("- dictionary_tightness_dovish.txt\n")
cat("- dictionary_supervision.txt\n")
cat("- dictionary_regulation.txt\n")


tone_counts <- tokens_clean %>%
  group_by(doc_id) %>%
  summarise(
    hawkish_hits = sum(stem %in% seed_hawkish),
    dovish_hits  = sum(stem %in% seed_dovish)
  )
view(tone_counts)
sum(tone_counts$hawkish_hits)


tone_counts <- tokens_clean %>%
  group_by(doc_id) %>%
  summarise(
    hawkish_hits = sum(stem %in% tightness_hawkish_stems),
    dovish_hits  = sum(stem %in% tightness_dovish_stems)
  )








