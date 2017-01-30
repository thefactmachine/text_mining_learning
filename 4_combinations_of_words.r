library(dplyr)
library(tidytext)
library(janeaustenr)

# Set n = 2 to divide into pairs of words
austen_digrams <- austen_books() %>%
  unnest_tokens(digram, text, token = "ngrams", n = 2)

austen_digrams

# notice how the token overlap. "sense and" is one token.  and "and sensibility" is another. 

austen_digrams %>% count(digram, sort = TRUE)

library(tidyr)
# lets split the two words into columns
diagrams_separated <- austen_digrams %>% separate(digram, c("word1", "word2"), sep = " ")

diagrams_filtered <- diagrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)

# now we can count the most common words
diagrams_filtered %>% count(word1, word2, sort = TRUE)

# we can now recombine the words
diagrams_united <- diagrams_filtered %>% tidyr::unite(diagram, word1, word2, sep = " ")

#===================================
#===================================
# Trigrams
#===================================

austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)


#===================================
# Digrams and tf_idf
#===================================

diagrams_united %>% count(book, diagram) %>% bind_tf_idf(diagram, book, n) %>% arrange(desc(tf_idf))


#===================================
# Using diagrams in sentiment analysis
#===================================
# The words happy and like will be counted as positive
# even in a sentence such as: "I am not happy"

diagrams_separated %>%
  dplyr::filter(word1 == "not") %>% 
  count(word1, word2, sort = TRUE)


# this just filters the sentiments data frame
AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, score)

AFINN

not_words <- diagrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  ungroup()
not_words


# ==== Which words contributed the most in the wrong direction...
# ===== To compute that we can multiple their score by the number of times
#===== they appear

df_graph <-  not_words %>%
              mutate(contribution = n * score) %>%
              arrange(desc(abs(contribution))) %>%
              head(20) %>% mutate(word2 = reorder(word2, contribution))

library(ggplot2)
p <- ggplot(df_graph, aes(word2, n * score, fill = score > 0))
p <- p + geom_bar(stat = "identity", show.legend = FALSE)
p <- p +xlab("Words preceded by \"not\"")
p <- p + ylab("Sentiment score * # of occurrences")
p <- p + coord_flip()
p
# ========================================
# === Other Negation words ===============
# ========================================

negation_words <- c("not", "no", "never", "without")

negated_words <- diagrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  ungroup()

negated_words


# ========================================
# === Graphs =============================
# ========================================


digram_counts <- diagrams_filtered %>% 
  count(word1, word2, sort = TRUE)
digram_counts

library(igraph)

# this filter restricts things to 77 rows.
digram_graph <- digram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

#  TODO: examples of igraph package
#  The igraph package has many powerful functions for manipulating and analyzing networks

library(ggraph)

set.seed(2016)

ggraph(digram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point()


set.seed(2016)

ggraph(digram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.1, "inches"))

ggraph(digram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


# =============================================
#==============================================
# Setting things up to make things repeatable.
count_digrams <- function(dataset) {
  dataset %>%
    unnest_tokens(digram, text, token = "ngrams", n = 2) %>%
    separate(digram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}


visualize_digrams <- function(digrams) {
  set.seed(2016)
  
  digrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

# ========== Now we can visualise things in King James Bible
library(gutenbergr)
kjv <- gutenberg_download(10)

kjv_digrams <- kjv %>%
  count_digrams()

kjv_digrams

kjv_digrams %>%
  filter(n > 40) %>%
  visualize_digrams()

# ===================================
#=======================================

#===== Counting and correllating pairs of words


austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

# this has a data.frame of 3 columns: book, section, word (37,246 rows)


# following installs widyr
# library(devtools)
#install_github("dgrtwo/widyr")


# The following shows the pairs of words
# that appear in the same section. 
# these are not n-grams
# we find that "elizabeth" and "darcy" are the most common pair
# this means that in Section1 for example. We might have: "elizabeth met with john and went and visited darcy"



word_pairs <- austen_section_words %>%
  widyr::pairwise_count(word, section, sort = TRUE)
word_pairs

# ===========================================
# PAIRWISE CORRELATION ======================
# ============================================

# Pairs like Elizabeth and Darcy are the most common co-occuring words
# but thats not that interesting since they are also the most common words

# TODO: formula for Pearson correlation, explanation of phi coefficient
# The pairwise_cor() function in widyr lets us perform a Pearson correlation across words.


library(widyr)

# We need to filter for at least relatively common words first
word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors

# we could find the words most correlated with "darcy"

word_cors %>% filter(item1 == "darcy")

# ============================================


# Just as we used ggraph to visualize digrams, we can use it to visualize correlations and 
# clusters among words that we’ve found through the widyr package.

word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()











































































































