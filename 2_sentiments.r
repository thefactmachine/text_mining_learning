

library(tidytext)

# this gives us the sentiments dataset
sentiments %>% head()

sentiments %>% group_by(lexicon, sentiment) %>% summarise(count = n())

# there are three lexicons here.  These are just different ways of 
# mapping unigrams to a specific sentiment.

# the AFINN lexicon assigns words with a score that runs between
# -5 and 5.

# not every word is in the lexicon as many words are neutral.
# these methods do not take into account qualifiers before a word
# such as "no good" or "not true"

# For many kinds of text..there are not sustained sections of 
# sarcarism or negated text.

# ========================================================
# ========== Sentiment Analysis with Inner Join ==========
# ========================================================

library(janeaustenr)
library(dplyr)
library(stringr)


tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

tidy_austen <- austen_books()
# see here: each row is a line of text (column names = "text", "book" (factor))
tidy_austen[300:320,]

# we have added line numbers for each book (unique for each book) and
# we have added chapter number for each book (non unique)
# the rows are lines of text (ie. multiple words)
tidy_austen_group <- tidy_austen %>% group_by(book) %>%
                      mutate(linenumber = row_number(), 
                             chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
                      ungroup()

# this breaks each line of text into a word
df_tidy_austen_words <- tidy_austen_group %>% unnest_tokens(word, text)
nrow(df_tidy_austen_words) == 725054

# okay lets subset the sentimentds data frame into words that indicate "joy"
df_nrc_joy <- sentiments %>% filter(lexicon == "nrc" & sentiment == "joy")
# we dont need the score column
df_nrc_joy$score <- NULL
head(df_nrc_joy)

#  ===========  DIFFERENCE BETWEEN SEMI JOIN and INNER JOIN ==========
# NUMBER OF ROWS ARE THE SAME. SEMI JOIN Just returns columns of first table
# WHILE Inner join returns all matching rows.

# ALl the following does is get a list of "joy" words from the book "EMMA"
df_sj_test <- df_tidy_austen_words %>% filter(book == "Emma") %>% 
                  semi_join(df_nrc_joy, by = c("word" = "word"))

df_ij_test <- df_tidy_austen_words %>% filter(book == "Emma") %>%
                  inner_join(df_nrc_joy, by = c("word" = "word"))


df_sj_count <- df_sj_test %>% count(word, sort = TRUE)

# =======================================================================
# How does sentiment change accross each novel
# =======================================================================

# select all columns except score
df_bing <- sentiments %>% filter(lexicon == "bing") %>% select(-score)

library(tidyr)
# this is the original code:
janeaustensentiment <- df_tidy_austen_words %>%
  inner_join(df_bing) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

head(janeaustensentiment)
nrow(janeaustensentiment) == 920


# this is my code

# add a column that indicates every 80 lines (integer division)
df_tidy_austen_words$index <- df_tidy_austen_words$linenumber %/% 80

# join with the df_bing
df_joined <- df_tidy_austen_words %>% inner_join(df_bing, by =("word" = "word"))
head(df_joined)

# sentiment is either positive or negative
df_joined_count <- df_joined %>% count(book, index, sentiment)
# now we have the following columns: book, index, sentiment (neg \ pos), n (ie. count)
head(df_joined_count)
nrow(df_joined_count) == 1840

df_joined_count_spread <- df_joined_count %>% spread(sentiment, n, fill = 0)
# this splits out sentiment into two additional columns: negative and positive
head(df_joined_count_spread)

df_joined_count_spread$sentiment <- df_joined_count_spread$positive - 
                                        df_joined_count_spread$negative


nrow(df_joined_count_spread) == 920

# =======================================================================
# =======================================================================

# graph stuff:
library(ggplot2)
ggplot(df_joined_count_spread, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

# =======================================================================
# 3.3 MOST COMMON POSITIVE AND NEGATIVE WORDS
# =======================================================================


df_joined <- df_tidy_austen_words %>% inner_join(df_bing, by = c("word" = "word"))
# this gives book, word, sentiment (ie. positive and negative)
head(df_joined)      

# need to ungroup here or else the resulting sort order is weird
df_agg <- df_joined %>% group_by(word, sentiment) %>% summarise(count = n()) %>% 
            ungroup() %>% arrange(desc(count))
head(df_agg)

library(ggstance)


df_agg %>%
  filter(count > 150) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_barh(stat = "identity") +
  xlab("Contribution to sentiment")

# reorder(character | factor, what to reorder). Returns a factor
df_graph_prep <- df_agg %>% 
                  filter(count > 150) %>%
                  mutate(n = ifelse(sentiment == "negative", -count, count)) %>%
                  mutate(word = reorder(word, n)) 

head(df_graph_prep)
library(ggstance)

p <- ggplot(df_graph_prep, aes(n, word, fill = sentiment))
p <- p +  ggstance::geom_barh(stat = "identity")
p <- p + xlab("Contribution to sentiment")
p

# =======================================================================
# 3.4 WORD CLOUDS
# =======================================================================
library(wordcloud)

# example of with()
ex_list <- list(a=1, b=2, c=3)
ex_list
with(ex_list, a + b + c)

df_wc_prep <- df_tidy_austen_words %>% anti_join(stop_words) %>% 
              count(word) %>% ungroup()
wordcloud(df_wc_prep$word, df_wc_prep$n, max.words = 100)

# =======================================================================
# 3.4 Comparison Clouds
# =======================================================================

library(reshape2)



# get rid of the underscores
df_tidy_austen_words$word <- str_extract(df_tidy_austen_words$word, "[a-z]+")

# this will give us positive and negative sentiment
df_tidy_austen_words_bing <- df_tidy_austen_words %>% inner_join(df_bing, by = c("word" = "word"))

df_tidy_austen_words_bing_count <- count(df_tidy_austen_words_bing, word, sentiment, sort = TRUE)

# prepare for comparision cloud
df_comp_cloud <- df_tidy_austen_words_bing_count %>% spread(sentiment, n, fill = 0)

# do not initially include the words in the matrix
mat_comp_cloud <- as.matrix(df_comp_cloud[, 2:3])
rownames(mat_comp_cloud) <- df_comp_cloud$word

comparison.cloud(mat_comp_cloud, colors = c("#F8766D", "#00BFC4"), max.words = 100)

# MOST COMMON POSITIVE AND NEGATIVE WORDS

# =======================================================================
# =======================================================================
# Uses acast from reshape2
# =======================================================================
# this is the original code from the on-line resource
library(reshape2)
test <- df_tidy_austen_words %>%
          inner_join(df_bing) %>%
          count(word, sentiment, sort = TRUE) %>%
          acast(word ~ sentiment, value.var = "n", fill = 0)
class(test)
dim(test)
rownames(test) %>% head()

comparison.cloud(test, colors = c("#F8766D", "#00BFC4"), max.words = 100)

# =======================================================================
# =======================================================================
# Looking at units beyond just words
# =======================================================================
# =======================================================================
PandP_sentences <- data_frame(text = prideprejudice) %>% 
  unnest_tokens(sentence, text, token = "sentences")

PandP_sentences$sentence[2]










