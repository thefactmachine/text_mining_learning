

# so far we have been using a tidy data set for our analysis.
# but many libraries in the: CRAN Task View for Natural Language Processing l
# make use of the document term matrix.  This is a sparse matrix of COUNTS
# with one row for each document and one column for each term.

# document term matrices are abbreviated as DTMs

# One commonly used implementation of DTMs in R is the DocumentTermMatrix class in the tm package. 

library(tm)
library(topicmodels)


data("AssociatedPress", package = "topicmodels")
class(AssociatedPress)

AssociatedPress

# ==========================
# ==========================
# === The first thing we are going to do is turn this matrix back into a data frame

library(dplyr)
library(tidytext)
ap_td <- tidy(AssociatedPress)
ap_td

# we now have three columns: document, term and count

# ==========================
# ==========================
# DOCUMENT FEATURE MATRICES

#  A tidier is also available for the dfm (document-feature matrix) class from the quanteda package
# Consider the corpus of presidential inauguration speeches that comes with the quanteda package:

library(methods)
library(quanteda)

data("inaugCorpus", package = "quanteda")
d <- quanteda::dfm(inaugCorpus)

d

library(broom)
broom::tidy(d) %>% head()

#  We could find the words most specific to several inaugural speeches using bind_tf_idf from chapter 4:

speeches <- c("1861-Lincoln", "1945-Roosevelt",
              "1961-Kennedy", "2009-Obama")

library(tidytext)
library(dplyr)


# Changed this to focus on Obama's speeches:

inaug_tf_idf_obama <- tidy(d) %>%
  tidytext::bind_tf_idf(term, document, count) %>%
  dplyr::filter(document %in% speeches[4]) %>%
  dplyr::arrange(desc(tf_idf))

inaug_tf_idf_obama

# ===========================================================
# CASTING TIDY TEXT DATA INTO A DOCUMENT TERM MATRIX (6.2)===
# ===========================================================
# some text mining tools work only on sparse document-term matrices

# we have the ap_td dataset.

ap_td

#this data set has columns: document, term, count


mat_ap <- ap_td %>% tidytext::cast_dtm(document, term, count)
str(mat_ap)
mat_ap

# the above is weird type of structure where the matrix is embedded into the 
# list

# some tools just require a sparse matrix
library(Matrix)
m <- ap_td %>%
  cast_sparse(document, term, count)

class(m)
dim(m)


# ===========================================================
# TIDYING CORPUS OBJECTS WITH METADATA (6.3)
# ===========================================================
reut21578 <- system.file("texts", "crude", package = "tm")

library(XML)

reuters <- VCorpus(DirSource(reut21578),
                   readerControl = list(reader = readReut21578XMLasPlain))



reuters

# The tidy verb creates a table with one row per document
reuters_td <- tidy(reuters)
reuters_td

# Another variation of a corpus object is corpus from the quanteda package:
library(quanteda)
data("inaugCorpus")
inaugCorpus

inaug_td <- tidy(inaugCorpus)
inaug_td
names(inaug_td)

inaug_words <- inaug_td %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


inaug_words$President %>% unique()

df_count_year <- inaug_words %>% count(Year, word) %>% ungroup()

library(tidyr)
# I think that this give frequencies of words for each each year.
# even if there are no words for say 2013...then code as zero.

df_using_complete <- df_count_year %>% 
  tidyr::complete(Year, word, fill = list(n = 0))

df_complete <-  df_using_complete %>% 
                mutate(year_total = sum(n), percent = n / year_total) %>% 
                ungroup()

# For instance, we could display the top 6 terms that have changed in frequency over time.
# BUT THE BIG QUESTION IS HOW TO WE GET THE TOP 6 TERMS.

library(ggplot2)
library(scales)
df_complete %>%
  filter(word %in% c("americans", "century", "foreign", "god",
                     "union", "constitution")) %>%
  ggplot(aes(Year, percent)) +
  geom_point(alpha = 0.8) +
  geom_smooth() +
  facet_wrap(~ word, scales = "free_y") +
  scale_y_continuous(labels = percent_format()) +
  ylab("Frequency of word in speech")





  
  
  
  
  





























































