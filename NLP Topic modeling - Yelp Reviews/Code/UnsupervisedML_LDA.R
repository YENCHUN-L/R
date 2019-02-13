# read in the libraries we're going to use
library(tidyverse) # general utility & workflow functions
library(tidytext) # tidy implimentation of NLP methods
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming
library(MASS)
for (i in c('SnowballC','slam','tm','RWeka','Matrix')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

library(readr)

reviews <- read_csv("C:/Users/yliu10/Desktop/SMA/Yelp_review_sample.csv", n_max=)
#access library


##############################################################################
############### Unsupervised ML ##############################################
##############################################################################

top_terms_by_topic_LDA <- function(input_text, # should be a columm from a dataframe
                                   plot = T, # return a plot? TRUE by defult
                                   number_of_topics = 4) # number of topics (4 by default)
{    
  # create a corpus (type of object expected by tm) and document term matrix
  Corpus <- Corpus(VectorSource(input_text)) # make a corpus object
  Corpus <- tm_map(Corpus, removePunctuation)
  Corpus <- tm_map(Corpus, removeNumbers)
  Corpus <- tm_map(Corpus, stripWhitespace)
  Corpus <- tm_map(Corpus,content_transformer(tolower))
  
  
  #remove stopwords
  Corpus <- tm_map(Corpus, removeWords,stopwords('english'))
  Corpus <- tm_map(Corpus, removeWords,c("will","get","just","like","one"
                                         ,"dont","ive","didnt","bit","wasnt","also","back","even","well"
                                         ,"best","definitely","ever","much","got","really","can","nice"
                                         ,"love","find","say","came","well","always","everything","try"
                                         ,"way","know","best","first","two","went","took","going","come"
                                         ,"sure","make","right","ordered","never","think","still","give"
                                         ,"think","said","little","ask","minutes","table","now","want"
                                         ,"told","next","day","years","around","made","asked","car"
                                         ,"order","take","need","called","lot","pretty","see","else"
                                         ,"since","many","another","pretty","wanted","tired","since","wait"
                                         ,"better"
                                         ,"every","times","new","amazing","thing","last","thing","recommend"
                                         ,"people","gave"
                                         ,"cant","though","work","thats","looking","bad","cant","tried","wrong"
                                         ,"side","awesome","found"
                                         ,"enough","server","something","youre","looked","visit","nothing","enough"
                                         ,"actually"
                                         ,"done","anything","away","something","hair","youre","things","long","felt"
                                         ,"hour","long","ill","check","use"
                                         ,"eat","used","almost","overall","put","disappointed","favorite","feel","however"
                                         ,"thought","end","later"
                                         ,"call","top","show","getting","busy","front","left","highly","half","coming"
                                         ,"maybe","usually"
                                         ,"without","wont"
  ))
  
  DTM <- DocumentTermMatrix(Corpus) # get the count of words/document
  
  # remove any empty rows in our document term matrix (if there are any 
  # we'll get an error when we try to run our LDA)
  unique_indexes <- unique(DTM$i) # get the index of each unique value
  DTM <- DTM[unique_indexes,] # get a subset of only those indexes
  
  # preform LDA & get the words/topic in a tidy text format
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  
  # get the top ten terms for each topic
  top_terms <- topics  %>% # take the topics data frame and..
    group_by(topic) %>% # treat each topic as a different group
    top_n(20, beta) %>% # get the top 10 most informative words
    ungroup() %>% # ungroup
    arrange(topic, -beta) # arrange words in descending informativeness
  
  # if the user asks for a plot (TRUE by default)
  if(plot == T){
    # plot the top ten terms for each topic in order
    top_terms %>% # take the top terms
      mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
      ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
      geom_col(show.legend = FALSE) + # as a bar plot
      facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
      labs(x = NULL, y = "Beta") + # no x label, change y label 
      coord_flip() # turn bars sideways
  }else{ 
    # if the user does not request a plot
    # return a list of sorted terms instead
    return(top_terms)
  }
}


###########################Run functions#################################

top_terms_by_topic_LDA(reviews$text, number_of_topics = 6)

#########################################################################