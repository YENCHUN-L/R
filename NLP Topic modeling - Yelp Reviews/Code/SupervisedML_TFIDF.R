#########################################
##########Tf-idf Supervised ML###########
#########################################

#read data
library(readr)
reviews <- reviews <- read_csv("C:/Users/yliu10/Desktop/SMA/Yelp_review_sample.csv", n_max=)

#################################
##########preprocessing##########
#################################

#Format to UTF-8
library(base)
Encoding(reviews$text) <- "UTF-8"

#Clean data
library(tm) # general text mining functions, making document term matrixes
reviews$text <- removePunctuation(reviews$text)
reviews$text <- removeNumbers(reviews$text)
reviews$text <- stripWhitespace(reviews$text)

#Standardized to lower case
reviews$text <- tolower(reviews$text)

#remove words
reviews$text <- removeWords(reviews$text, stopwords("english")) 
reviews$text <- removeWords(reviews$text, c("centurylink","mbps","limitword","josef","lashley","texturize"
                                            ,"clep","wuz","uuu","drawback","jpg","lockaid","insectek","limitWord"
                                            ,"ino","celp","jpg"))

#Break useful funny cool to Positive and Negative
reviews$useful <- ifelse(reviews$useful == 0 , "Not Useful", "Useful")
reviews$funny <- ifelse(reviews$funny == 0 , "Not Funny", "Funny")
reviews$cool <- ifelse(reviews$cool == 0 , "Not Cool", "Cool")


###################################
##########Tf-idf Function##########
###################################

library(rlang)
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyverse)
top_terms_by_topic_tfidf <- function(text_df, text_column, group_column, plot = T){
  # name for the column we're going to unnest_tokens_ to
  # (you only need to worry about enquo stuff if you're
  # writing a function using using tidyverse packages)
  group_column <- enquo(group_column)
  text_column <- enquo(text_column)
  
  # get the count of each word in each review
  words <- text_df %>%
    unnest_tokens(word, !!text_column) %>%
    count(!!group_column, word) %>% 
    ungroup()
  
  # get the number of words per text
  total_words <- words %>% 
    group_by(!!group_column) %>% 
    summarize(total = sum(n))
  
  # combine the two dataframes we just made
  words <- left_join(words, total_words)
  
  # get the tf_idf & order the words by degree of relevence
  tf_idf <- words %>%
    bind_tf_idf(word, !!group_column, n) %>%
    select(-total) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word))))
  
  if(plot == T){
    # convert "group" into a quote of a name
    # (this is due to funkiness with calling ggplot2
    # in functions)
    group_name <- quo_name(group_column)
    
    # plot the 10 most informative terms per topic
    tf_idf %>% 
      group_by(!!group_column) %>% 
      top_n(10) %>% 
      ungroup %>%
      ggplot(aes(word, tf_idf, fill = as.factor(group_name))) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(reformulate(group_name), scales = "free") +
      coord_flip()
  }else{
    # return the entire tf_idf dataframe
    return(tf_idf)
  }
}


########################################
##########Run tf-idf functions##########
########################################

# Most informative words for stars
top_terms_by_topic_tfidf(text_df = reviews, # dataframe
                         text_column = text, # column with text
                         group_column = stars, # column with topic label
                         plot = T) # return a plot

# Most informative words for useful and not useful
top_terms_by_topic_tfidf(text_df = reviews, # dataframe
                         text_column = text, # column with text
                         group_column = useful, # column with topic label
                         plot = T) # return a plot

# Most informative words for funny and not funny
top_terms_by_topic_tfidf(text_df = reviews, # dataframe
                         text_column = text, # column with text
                         group_column = funny, # column with topic label
                         plot = T) # return a plot

# Most informative words for cool and not cool
top_terms_by_topic_tfidf(text_df = reviews, # dataframe
                         text_column = text, # column with text
                         group_column = cool, # column with topic label
                         plot = T) # return a plot