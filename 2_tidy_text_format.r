

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
)
text

library(dplyr)
df_text <- dplyr::data_frame(line = 1:length(text),
                             text = text)

df_text

library(tidytext)
# creates a two column data.frame.
# strips out punctuation and converts the words to lower_case
# number of rows = number of distinct words
# the lower case conversion can be turned off: to_lower = FALSE
df_text %>% tidytext::unnest_tokens(word, text)


library(janeaustenr)
library(dplyr)
library(stringr)



# df_austen_books 
df_austen_books <- austen_books()
# each row is a line
head(df_austen_books, 15)

# the input to str_detect is a character vector
# regex square brackets means any character
# /d is short for 0-9
# string_detect returns a boolean vector
# text here is the column name
original_books <- df_austen_books %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>% ungroup()

# this is equal to the number of lines in the books
nrow(original_books) == 73422

# this is now 725,054
# this uses the tokenizers package. And uses the word token
# other options are characters, n-grams, sentences, lines, regex...
# "text" here is a column name of original_books
df_tidy_books <- original_books %>% unnest_tokens(word, text)

# stop_words is a 2 column data_frame with the columns: "word" and "lexicon"
data("stop_words")

# nrow(217609)
tidy_books_sans_sw <- tidy_books %>% anti_join(stop_words, by =  c("word" = "word"))

# limit output to the first 20 rows. 
df_graph_disp <- tidy_books_sans_sw %>% 
                  group_by(word) %>% 
                  summarise(count = n()) %>%
                  arrange(desc(count)) %>%
                  filter(row_number() < 20 )

library(ggplot2)
# need to play around with the factor order to 
# change order of the bars.
x <- ggplot(df_graph_disp, aes(word, count))
x <- x + geom_bar(stat = "identity")
x

#================================
# 2.3 gutenbergr package is missing
#================================

#================================
# 2.4 word frequencies
#================================

# We calculate the word frequencies for
# HG Well and Jane Austin books

library(gutenbergr)
library(dplyr)
library(stringr)
library(tidytext)

hgwells <- gutenberg_download(c(35, 36, 5230, 159))
tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


tidy_hgwells %>%
  count(word, sort = TRUE)

# now we get some works from the Bronte Sisters
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 766))
tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_bronte %>%
  count(word, sort = TRUE)

# combine both data sets
tidy_bronte <- tidy_bronte %>% mutate(author = "Brontë Sisters")
head(tidy_bronte,2)

tidy_hgwells <- tidy_hgwells %>% mutate(author = "H.G. Wells")
head(tidy_hgwells,2)

tidy_both <- bind_rows(tidy_bronte, tidy_hgwells)

vct_word_test <- c("mark", "allan", "_hatcher", "at", "thefactmachine_", "yuk_ari")
vct_word_test
# here we are excluding all the pieces that are NOT part of the pattern
str_extract(vct_word_test, "[a-z]+")

# We use str_extract here because the UTF-8 encoded texts from Project Gutenberg 
# have some examples of words with underscores around them to indicate 
# emphasis (like italics). The tokenizer treated these as words but 
# we don’t want to count “_any_” separately from “any”. Now let’s plot.

tidy_both <- tidy_both %>% mutate(word = str_extract(word, "[a-z]+"))

# see here that the count is labelled as "n"
tidy_both <- tidy_both %>% count(author, word)
head(tidy_both)

# rename "n" to become "other"
tidy_both <- tidy_both %>% rename(other = n)
head(tidy_both)

# Assume the script above has run...and we have the following to work with
head(tidy_books_sans_sw)
# get rid of "_'
tidy_books_sans_sw <- tidy_books_sans_sw %>% mutate(word = str_extract(word, "[a-z]+"))

# do a count
tidy_books_sans_sw <- tidy_books_sans_sw %>% count(word)

# rename n to "Austen"
tidy_books_sans_sw <- tidy_books_sans_sw %>% rename(Austen = n)

# ungroup things
# columns are "word" and "Austen (which is a count of words"
tidy_books_sans_sw <- tidy_books_sans_sw %>% ungroup()

# columns are "author ("Bronte Sisters" and "H.G. Wells"), word, and "other" (count)
tidy_both <- tidy_both %>% ungroup()

# club both together on common words
frequency <- inner_join(tidy_books_sans_sw, tidy_both, by = c("word" = "word"))

frequency <-  frequency %>% mutate(other = other / sum(other), Austen = Austen / sum(Austen))

library(ggplot2)
library(scales)

ggplot(frequency, aes(x = other, y = Austen, color = abs(Austen - other))) +
  geom_abline(color = "red") +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.4, height = 0.4) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  #  scale_color_gradient(limits = c(0, 0.001), low = "gray30", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

# so the way to interpret these graphs are:
# there are two scatter plots:
# 1) Jane Austin Vs Bronte Sisters
# 2) Jane Austin Vs HG Wells
# points close to the 45 degree line are those which have
# a similar relative frequency to those in each pair-wise combination
# notice that words in Austin Vs Bronte (left) have a much closer spread (more correllated)
# than words in Austin Vs HG Wells.

# other in this case is the "Bronte Sister" 0.7549041
cor.test(data = frequency[frequency$author == "Brontë Sisters",], ~ other + Austen)

# other in this case is the "HG Wells" 0.4198191 
cor.test(data = frequency[frequency$author == "H.G. Wells",], ~ other + Austen)

# here is another way....
# all we are doing is a correlation of the relative frequencies.
df_bronte <- frequency[frequency$author == "Brontë Sisters", ]
df_hg <- frequency[frequency$author == "H.G. Wells", ]

cor(df_bronte$other, df_bronte$Austen)
cor(df_hg$other, df_hg$Austen)


tidy_both_orig <- bind_rows(
  mutate(tidy_bronte, author = "Brontë Sisters"),
  mutate(tidy_hgwells, author = "H.G. Wells"))
  frequency <- tidy_both %>%
  mutate(word = str_extract(word, "[a-z]+")) %>%
  count(author, word) %>%
  rename(other = n) %>%
  inner_join(count(tidy_books_sans_sw, word)) %>%
  rename(Austen = n) %>%
  mutate(other = other / sum(other),
         Austen = Austen / sum(Austen)) %>%
  ungroup()





