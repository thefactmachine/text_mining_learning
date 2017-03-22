# df_austen_books 
df_austen_books <- austen_books()

original_books <- df_austen_books %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>% ungroup()

df_tidy_books <- original_books %>% unnest_tokens(word, text)

data("stop_words")

tidy_books_sans_sw <- df_tidy_books %>% anti_join(stop_words, by =  c("word" = "word"))

