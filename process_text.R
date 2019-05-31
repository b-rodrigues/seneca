library(tidyverse)
library(tidytext)

letters_to_lucilius <- readRDS("letters_to_lucilius.rds")


letters_to_lucilius_df <- map(letters_to_lucilius, ~tibble("text" = .))

letters_to_lucilius_df %>%
  map(~slice(., 1))
