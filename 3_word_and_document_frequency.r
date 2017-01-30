

rm(list = ls())

# td / idf see notes.

library(dplyr)
library(janeaustenr)
library(tidytext)

# count of words by book
book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()

# there are six books
unique(book_words$book)

# words for each book
total_words <-  book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)
book_words %>% head()

# calculate relative frequency
book_words$rel_freq <- book_words$n / book_words$total


library(ggplot2)

# create a historgram.  All books have long tails
ggplot(book_words, aes(rel_freq, fill = book)) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  xlim(NA, 0.0009) +
  labs(title = "Term Frequency Distribution in Jane Austen's Novels") +
  facet_wrap(~book, ncol = 3, scales = "free_y")


# word is the word, book is the factor, n = number of words in the book.
# if the word is in all books then its idf term will be zero as the log of 1 is zero
# this will make the tf-idf term zero.
book_words <- book_words %>% bind_tf_idf(word, book, rel_freq)
book_words

book_words %>% arrange(desc(tf_idf))
# notice here how the words are proper nouns. 

#  My favorite one is "that TF-IDF measures how surprising 
# it is to see the query keywords in a document given a 
# known distribution of these keywords in the text collection."

# perhaps another interpretation is how certain words can be used to
# identify a particular corpus.

# graphs and other stuff not done.








