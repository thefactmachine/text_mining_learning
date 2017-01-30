

# THE SITUATION:
# A person has broken into your study and taken four books.
# and has torn them apart into chapters. All these chapters are
# scattered around the floor.  How can restore the chapters into the 
# original books.

library(dplyr)
titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")

library(gutenbergr)
books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

# The resulting data.frame has three columns: gutenberg_id (4 unqiue), text, title
# The titel only has four unique values.

library(stringr)

# This clever piece of code adds an extra column:



by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(stringr::str_detect(text, stringr::regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0)

sum(by_chapter$chapter) == 1421465
nrow(by_chapter) == 51602


# if you dont understand the code above...look at this:
# here is another ways of doing that mutate above:
reg_exp <- stringr::regex("^chapter ", ignore_case = TRUE)
# following returns a boolean.  "TRUE" ==> that the line started with "chapter"
vct_bool <- stringr::str_detect(books$text, reg_exp)
length(vct_bool) == length(books$text)
vct_cum_sum <- base::cumsum(vct_bool)




library(tidyr)

# this replaces the column title and chapter with "title_chapter" .... 
# which is a concatenation of title and chapter
by_chapter_word_unite <- by_chapter %>%
  tidyr::unite(title_chapter, title, chapter)


by_chapter_word_unite

library(tidytext)
by_chapter_word  <- by_chapter_word_unite %>% tidytext::unnest_tokens(word, text)
by_chapter_word

# this contains the stop words
library(tm)

# here is a count of chapter AND words
word_counts <- by_chapter_word %>%
  anti_join(tidytext::stop_words) %>%
  count(title_chapter, word, sort = TRUE) %>%
  ungroup()

word_counts

# ========================================================================
# ========================================================================
# LATENT DIRICHLET ALLOCATION WITH THE TOPICMODELS PACKAGE
# ========================================================================

# the topicmodels package requires a DocumentTermMatrix (fromt the tm package)

library(tm)
chapters_dtm <- word_counts %>%
  tidytext::cast_dtm(title_chapter, word, n)

chapters_dtm

#  Now we are ready to use the topicmodels package to create a four topic LDA model.
library(topicmodels)

# takes about 15 secs to run.
chapters_lda <- topicmodels::LDA(chapters_dtm, k = 4, control = list(seed = 1234))


# In this case we know there are four topics because there are four books; 
# in practice we may need to try a few different values of k).

typeof(chapters_lda) == "S4"

#  It is typically a design mistake to use ::: in your code since the 
# corresponding object has probably been kept internal for a good reason

library(tidytext)
# This: Tidy the results of a Latent Dirichlet Allocation.
chapters_lda_td <- tidytext:::tidy.LDA(chapters_lda)
chapters_lda_td

# the term Beta is the probability of that term being generated from that topic.

# We could use dplyr’s top_n to find the top 5 terms within each topic:

top_terms <- chapters_lda_td %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

library(ggplot2)


top_terms <- top_terms %>% mutate(term = reorder(term, beta))
g <- ggplot(top_terms, aes(term, beta, fill = factor(topic))) 
g <- g + geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) 
# following is the original but this does not seem to work
# g <- g + coord_flip()
g <- g + facet_wrap(~ topic, scales = "free")
g

# ===================================================================
#  7.3 Per-document classification
# ===================================================================

# this is our model:
chapters_lda

# previously we had this:
# This: Tidy the results of a Latent Dirichlet Allocation.
# this gives a probability of each term being in each topic
chapters_lda_td <- tidytext:::tidy.LDA(chapters_lda)
chapters_lda_td


# this gives a probablity of each document/chapter combination being in each topic
chapters_lda_gamma <- tidytext:::tidy.LDA(chapters_lda, matrix = "gamma")
chapters_lda_gamma


# this following just splits up the column document into two columns
chapters_lda_gamma <- chapters_lda_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)
chapters_lda_gamma


# mmm I dont really know how to read the following:
ggplot(chapters_lda_gamma, aes(gamma, fill = factor(topic))) +
  geom_histogram() + facet_wrap(~ title, nrow = 2)

# We notice that almost all of the chapters  were identified as a single topic each

# firstly lets understand this data frame a little bit better:

chapters_lda_gamma %>% filter(title == "Great Expectations") %>% arrange(chapter)

# use unique with a data frame not a vector for multiple columns
unique(chapters_lda_gamma[, c(1,2)]) %>% nrow() 

# multiply by the number of topics
(unique(chapters_lda_gamma[, c(1,2)]) %>% nrow() * 4) == nrow(chapters_lda_gamma)

# this finds the top gamma for each title / chapter combination
chapter_classifications <- chapters_lda_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup() %>%
  arrange(gamma)

chapter_classifications

# now which was the most popular topic for each book.
# count adds a column called n

book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  top_n(1, n) %>%
  ungroup()

book_topics

# Mutate adds new variables and preserves existing; transmute drops existing variables.
book_topics_new <- book_topics %>% transmute(consenus = title, topic)
book_topics_new


# lets see which chapters were incorrectly classified
chapter_classifications %>%
  inner_join(book_topics_new, by = c("topic" = "topic")) %>% filter(consenus != title)

# ========================================
# 7.4 BY Word Assignment
# ========================================

# augment lists each word and which topic is was originally a member of
assignments <- tidytext:::augment.LDA(chapters_lda, data = chapters_dtm)
assignments

book_topics <- book_topics %>% rename(consensus = title)


assignments <- assignments %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
  inner_join(book_topics, by = c(".topic" = "topic"))


assignments_count <- assignments %>% count(title, consensus, wt = count)


assignments_count %>% tidyr::spread(consensus, nn, fill = 0)

# which were the most commonly mistaken words:
wrong_words <- assignments %>%
  filter(title != consensus) %>% 
    group_by(term) %>% 
      summarise(total = sum(count)) %>% arrange(desc(total))

wrong_words










