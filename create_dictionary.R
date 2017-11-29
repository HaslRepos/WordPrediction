enBlogs   <- readLines(file.path('./Dataset/final/en_US/', 'en_US.blogs.txt'), encoding="UTF-8", skipNul=TRUE)
enNews    <- readLines(file.path('./Dataset/final/en_US/', 'en_US.news.txt'), encoding="UTF-8", skipNul=TRUE)
enTwitter <- readLines(file.path('./Dataset/final/en_US/', 'en_US.twitter.txt'), encoding="UTF-8", skipNul=TRUE)

set.seed(8872)
enSample <- c(sample(enBlogs, length(enBlogs) * 0.01),
              sample(enNews, length(enNews) * 0.01),
              sample(enTwitter, length(enTwitter) * 0.01)
             )

library(tm)
enCorpus <- VCorpus(VectorSource(enSample))
enCorpus <- tm_map(enCorpus, content_transformer(function(x) iconv(x, to="ASCII", sub = "")))
enCorpus <- tm_map(enCorpus, tolower)
enCorpus <- tm_map(enCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
enCorpus <- tm_map(enCorpus, removeNumbers)
enCorpus <- tm_map(enCorpus, stripWhitespace)
enCorpus <- tm_map(enCorpus, removeWords, readLines('./Dataset/swearWords.txt'))
enCorpus <- tm_map(enCorpus, PlainTextDocument)

library(RWeka)
token2g <- function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))}
token3g <- function(x) {NGramTokenizer(x, Weka_control(min = 3, max = 3))}
token4g <- function(x) {NGramTokenizer(x, Weka_control(min = 4, max = 4))}
tdm2 <- TermDocumentMatrix(enCorpus, control=list(tokenize=token2g, wordLengths = c(1, Inf)))
tdm3 <- TermDocumentMatrix(enCorpus, control=list(tokenize=token3g, wordLengths = c(1, Inf)))
tdm4 <- TermDocumentMatrix(enCorpus, control=list(tokenize=token4g, wordLengths = c(1, Inf)))

library(tidytext)
tdm2_td <- tidy(tdm2)
tdm3_td <- tidy(tdm3)
tdm4_td <- tidy(tdm4)

library(dplyr)
library(tidyr)
library(stringr)

dict2 <- tdm2_td  %>% filter(!str_detect(term, "( |^)(-)+|-+( |$)")) %>% count(term, wt=count) %>% separate(term, c("term", "predict"), sep = " ") %>% group_by(term) %>% mutate(freq = n / sum(n)) %>% top_n(n = 5, wt = n) %>% arrange(term, desc(n)) %>% ungroup()

saveRDS(dict2, "./dictionary/dictionary2EN.RDS", ascii=FALSE, compress=TRUE)

dict3 <- tdm3_td  %>% filter(!str_detect(term, "( |^)(-)+|-+( |$)")) %>% count(term, wt=count) %>% separate(term, c("term1", "term2", "predict"), sep = " ") %>% unite(term, term1, term2, sep = " ") %>% group_by(term) %>% mutate(freq = n / sum(n)) %>% top_n(n = 5, wt = n) %>% arrange(term, desc(n)) %>% ungroup()

saveRDS(dict3, "./dictionary/dictionary3EN.RDS", ascii=FALSE, compress=TRUE)

dict4 <- tdm4_td  %>% filter(!str_detect(term, "( |^)(-)+|-+( |$)")) %>% count(term, wt=count) %>% separate(term, c("term1", "term2", "term3", "predict"), sep = " ") %>% unite(term, term1, term2, term3, sep = " ") %>% group_by(term) %>% mutate(freq = n / sum(n)) %>% top_n(n = 5, wt = n) %>% arrange(term, desc(n)) %>% ungroup()

saveRDS(dict4, "./dictionary/dictionary4EN.RDS", ascii=FALSE, compress=TRUE)
