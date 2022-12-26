## thesis wordcloud
## https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a

library(pdftools)
library(wordcloud)
library(wordcloud2)
library(tm)
library(dplyr)
`%nin%` <- Negate(`%in%`)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

my_pdf <- readPDF(control=list(text="-layout"))(elem=list(uri="C:/Users/adrie/OneDrive/Desktop/Thesis/Thesis.pdf"), language="en")
text_raw <- my_pdf$content
text_raw <- text_raw[-c(1:3, 6)] 
text_raw <- text_raw[-c(5:18)] 
text_raw <- text_raw[-c(115:240)]

text_corpus <- Corpus(VectorSource(text_raw))
corpus_clean <- tm_map(text_corpus, 
                       removePunctuation, 
                       preserve_intra_word_dashes = TRUE, 
                       preserve_intra_word_contractions = TRUE)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
#corpus_clean <- tm_map(text_corpus, stripWhitespace)
corpus_clean <- tm_map(corpus_clean, content_transformer(tolower))
corpus_clean <- tm_map(corpus_clean, toSpace, "\\-")


corpus_clean <- tm_map(corpus_clean, removeWords, stopwords("english"))
my_stopwords <- c("can","due","will", # additional user-defined stop words
                  "figure","figures","figure","online", # stop words related to figure captions
                  "work", "paper", "doi", "package", "appendix", "also", "using", 
                  "chapter", "across", "table", "likely", 
                  "found", "used", "associated", "well", "shown", 
                  "results", "trait", "one", "thus", "land", "uses", "-use", 
                  "whether", "investigated", "main", "considered", 
                  "important", "although", "among", "assess", "available", 
                  "changes", "class", "climate", "database", "different",
                  "effect", "energetic", "estimated", "given", "however", "influence", 
                  "instance", "less", "levels", "level", "lower", "may", "model",
                  "newbold", "overall", "reference", "rmr", "sensitive", "showed", "sources", 
                  "total", "two", "vertebrate", "within") # stop words related to the journal
corpus_clean <- tm_map(corpus_clean, removeWords, my_stopwords)

# Create a document-term-matrix
dtm <- TermDocumentMatrix(corpus_clean) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
rownames(df) <- c(1:nrow(df))


## cleaning the frequency table
df <- df %>% 
  filter(word %nin% 
           c("−", "species’", "body", "primary", "areas", "-change", "diversity", "higher", 
             "similar", "size", "values", "predicts", "groups", "types", "number", "breadth",
             "non", "food", "gain", "group", "significant", "median", "large", "past", "estimates", 
             "average"), freq>30)

df$word[df$word=="use"] <- "land use"
df$word[df$word=="change"] <- "climate change"

# set.seed(1234) # for reproducibility 
# wordcloud(words = df$word, freq = df$freq, min.freq = 1,       
#           max.words=200, random.order=FALSE, rot.per=0.35,          
#           colors=brewer.pal(8, "Dark2"), scale=c(3.5,0.25) )
wordcloud2(data=df, size=1, color='random-light', minSize = 1, backgroundColor = "black", gridSize = 1)

df <- data.frame(word = names(words),freq=words)
letterCloud(df, word = "Thank you!",
            color='random-light', 
            backgroundColor = "black")
