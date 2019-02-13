library("ldatuning")
library("tm")
library("readr")
reviews <- read_csv("C:/Users/yliu10/Desktop/SMA/Yelp_review_sample.csv", n_max=)

Corpus <- Corpus(VectorSource(reviews)) # make a corpus object
Corpus <- tm_map(Corpus, removePunctuation)
Corpus <- tm_map(Corpus, removeNumbers)
Corpus <- tm_map(Corpus, stripWhitespace)
Corpus <- tm_map(Corpus,content_transformer(tolower))


#remove stopwords
Corpus <- tm_map(Corpus, removeWords,stopwords('english'))
Corpus <- tm_map(Corpus, removeWords,c("will","great","get","just","like","one","good"
                                       ,"dont","ive","didnt","bit","wasnt",""))

dtm <- DocumentTermMatrix(Corpus) # get the count of words/document

# remove any empty rows in our document term matrix (if there are any 
# we'll get an error when we try to run our LDA)
unique_indexes <- unique(dtm$i) # get the index of each unique value
dtm <- dtm[unique_indexes,] # get a subset of only those indexes



result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c( "Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 20L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)


# https://www.rdocumentation.org/packages/ldatuning/versions/0.2.0/topics/FindTopicsNumber
# https://quantdev.ssri.psu.edu/sites/qdev/files/topic_modeling_tutorial-Gutenberg-chapter_as_document_0.html
# https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html
# https://www.pnas.org/content/101/suppl_1/5228
