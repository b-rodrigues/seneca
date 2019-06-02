library(tidyverse)
library(tidytext)
library(SnowballC)
library(stopwords)
library(text2vec)
library(dbscan)

letters_to_lucilius <- readRDS("letters_to_lucilius.rds")

letters_to_lucilius_df <- map(letters_to_lucilius, ~tibble("text" = .))

letter_titles <- letters_to_lucilius_df %>%
  map(~slice(., 1)) %>%
  map(pull)

letters_to_lucilius_df <-  map2(.x = letters_to_lucilius_df, .y = letter_titles,
                                ~mutate(.x, title = .y)) %>%
  map(~slice(., -1))

tokenized_letters <- letters_to_lucilius_df %>%
  bind_rows() %>%
  group_by(title) %>%
  unnest_tokens(word, text)

stopwords_en <- tibble("word" = stopwords("en", source  = "smart"))

tokenized_letters <- tokenized_letters %>%
  anti_join(stopwords_en) %>%
  filter(!str_detect(word, "\\d{1,}"))

tokenized_letters <- tokenized_letters %>%
  mutate(word = wordStem(word, language = "en"))

tfidf_letters <- tokenized_letters %>%
  count(title, word, sort  = TRUE) %>%
  bind_tf_idf(word, title, n)


#test <- tfidf_letters %>%
#  ungroup() %>%  
#  slice(1:5000)

sparse_training <- tfidf_letters %>%
  cast_sparse(title, word, tf)

similarities <- sim2(sparse_training, method = "cosine", norm = "l2") 

get_similar_letters <- function(similarities, reference_article, n_recommendations = 3){
  sort(similarities[reference_article, ], decreasing = TRUE)[1:(2 + n_recommendations)]
}

get_similar_letters(similarities, 19)
get_similar_letters(similarities, 99)
get_similar_letters(similarities, 32)
get_similar_letters(similarities, 87)
