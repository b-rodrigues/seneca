library(tidyverse)
library(tidytext)
library(SnowballC)

letters_to_lucilius <- readRDS("letters_to_lucilius.rds")

letters_to_lucilius_df <- map(letters_to_lucilius, ~tibble("text" = .))

letter_titles <- letters_to_lucilius_df %>%
  map(~slice(., 1)) %>%
  map(pull)

letters_to_lucilius_df <-  map2(.x = letters_to_lucilius_df, .y = letter_titles,
     ~mutate(.x, title = .y))

tokenized_letters <- letters_to_lucilius_df %>%
  bind_rows() %>%
  group_by(title) %>%
  unnest_tokens(word, text)

stopwords_en <- tibble("word" = stopwords::stopwords("en", source  = "smart"))

tokenized_letters <- tokenized_letters %>%
  anti_join(stopwords_en) %>%
  filter(!str_detect(word, "\\d{1,}"))

tfidf_letters <- tokenized_letters %>%
  count(title, word, sort  = TRUE) %>%
  bind_tf_idf(word, title, n)


tfidf_letters %>%
  arrange(desc(tf_idf))
