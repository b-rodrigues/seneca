library(tidyverse)
library(rvest)

base_url <- "https://en.wikisource.org/wiki/Moral_letters_to_Lucilius/Letter_"

letter_numbers <- seq(1, 124)

letter_urls <- paste0(base_url, letter_numbers)

get_raw_text <- function(base_url, letter_number){
  paste0(base_url, letter_number) %>%
    read_html() %>%
    html_nodes(xpath ='//*[contains(concat( " ", @class, " " ), concat( " ", "mw-parser-output", " " ))]') %>%  
    html_text()
}


extract_text <- function(raw_text, letter_number){
  raw_text <- raw_text %>%
    str_split("\n") %>%  
    flatten_chr() %>%  
    discard(~`==`(., ""))

  cat("hello")

  start <- case_when(
    letter_number == 87 ~ str_which(raw_text, "LXXXVII. Some Arguments in Favour of the Simple Life"),
    letter_number == 120 ~ str_which(raw_text, "CXX. More about Virtue"),
    TRUE ~ str_which(raw_text, paste0("^",as.roman(letter_number), ". (On)"))
  )

  cat("start", start, "\n")
  #start <- str_which(raw_text, paste0("^",as.roman(letter_number), ". (On)"))

  end <- str_which(raw_text, "Footnotes*")


  raw_text[start:(end-1)] %>%
    str_remove_all("\\[\\d{1,}\\]") %>%
    str_remove_all("\\[edit\\]")
}

get_letter <- function(base_url, letter_number){

  cat("getting letter: ", letter_number, "\n")

  raw_text <- get_raw_text(base_url, letter_number)

  extract_text(raw_text, letter_number)
}


letters_to_lucilius <- map2(base_url, letter_numbers, get_letter)

#saveRDS(letters_to_lucilius, "letters_to_lucilius.rds")
