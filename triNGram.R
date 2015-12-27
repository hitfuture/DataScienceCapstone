s <- "The quick brown fox jumps over the lazy dog"
## Split into words:
w <- strsplit(s, " ", fixed = TRUE)[[1L]]
## Word tri-grams:
ngrams(w, 3L)
## Word tri-grams pasted together:
vapply(ngrams(w, 3L), paste, "", collapse = " ")


library("RWeka")
library("tm")
library(wordcloud)

data("crude")
options(mc.cores=1)
UnigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))
tdm1 <- TermDocumentMatrix(crude, control = list(tokenize = UnigramTokenizer))
#tdm1 <- removeSparseTerms(tdm1, 0.75) 
plot(tdm1)

BigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))
tdm2 <- TermDocumentMatrix(crude, control = list(tokenize = BigramTokenizer))
plot(tdm2)
#tdm2 <- removeSparseTerms(tdm2, 0.75) 
 
TrigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))
tdm3 <- TermDocumentMatrix(crude, control = list(tokenize = TrigramTokenizer))
plot(tdm3 )
#tdm3 <- removeSparseTerms(tdm3, 0.75)
inspect(tdm1)
inspect(tdm2)
inspect(tdm3 )


termDoc  <- tdm1
m <- as.matrix(termDoc)
v <- sort(rowSums(m),decreasing = TRUE)
d <- data.frame(word = names(v),freq=v)
pal <- brewer.pal(6,"Dark2")
pal <- pal[-(1)]

wordcloud(d$word, d$freq,scale = c(4,0.4),colors =  pal)



termDoc  <- tdm2
m <- as.matrix(termDoc)
v <- sort(rowSums(m),decreasing = TRUE)
d <- data.frame(word = names(v),freq=v)
pal <- brewer.pal(6,"Dark2")
pal <- pal[-(1)]
wordcloud(d$word, d$freq,scale = c(4,0.4),colors =  pal)



termDoc  <- tdm3
m <- as.matrix(termDoc)
v <- sort(rowSums(m),decreasing = TRUE)
d <- data.frame(word = names(v),freq=v)
pal <- brewer.pal(6,"Dark2")
pal <- pal[-(1)]

wordcloud(d$word, d$freq,scale = c(4,0.4),colors =  pal)
