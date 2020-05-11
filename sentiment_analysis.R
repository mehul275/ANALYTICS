install.packages('tidytext')
install.packages('textdata')
library(textdata)    #text analysis
library(tidytext)    #text processing capabilities that are optimized for performance critical use

get_sentiments('bing')    #negative/positive
get_sentiments('nrc')     #happy,sad,etc
get_sentiments('afinn')   #(-5,5)

library(janeaustenr)    #an author
library(stringr)       # Character manipulation
library(dplyr)         #select and filter

edit(austen_books())   #to view+edit book

tidy_data <- austen_books() %>% group_by(book) %>%
              mutate(linenumber = row_number(),
  chapter = cumsum(str_detect( text,  regex("^chapter[\\divx]",ignore_case = T) ) )) %>% #??
  ungroup() %>% unnest_tokens(word, text)

edit(tidy_data)
# edit(tidy_data)
# line no column will include that no of row which contain text; detecting to exclude 
# string "chapter" succeeding digits,roman no with ignoring case sensitivity 
#  separating text into words 


#making two sets of positive and negative sentiments of whole book 
positive_sentiments <- get_sentiments('bing') %>%  #filtering only positive sentiments
    filter(sentiment == "positive")
View(positive_sentiments)

negative_sentiments <- get_sentiments('bing') %>%  #filtering only positive sentiments
    filter(sentiment == "negative")
View(negative_sentiments)


tidy_data %>%   #filtering emma; joining only positive sentiments; counting word; arranging in desc 
  filter(book == "Emma") %>% semi_join(positive_sentiments) %>% count(word , sort = T)

edit(tidy_data)             #count(data,condition)


#to plot positive/negative  sentiments

library(tidyr)                   #missing values,gather(),spread()
bing <- get_sentiments("bing")   #have sentiments like positive/negative
View(bing)                      #iimporting bing for futher comparisons with emma book

Emma_sentiment <- tidy_data %>%
  inner_join(bing) %>%     #inner_join() would join bing ; tells that sentiment (poitive/negative)
  count(book = "Emma", index = linenumber %/% 80, sentiment)%>% #index finds each 80th line
  spread(sentiment, n, fill = 0) %>%    # will spread each 80th line into tokens
  mutate(sentiment = positive - negative) #a new col containing score

edit(Emma_sentiment)

#plotting
library(ggplot2)

ggplot(Emma_sentiment, aes(index,sentiment, fill = book)) + 
  geom_bar(stat = 'identity') 


counting_words <- tidy_data %>% 
  inner_join(bing) %>% 
  count(word, sentiment, sort = T)

head(counting_words)

counting_words %>% 
  filter(n > 150) %>% 
  mutate(n = ifelse(sentiment == 'negative', -n, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() + 
  coord_flip() +
  labs(y = 'Sentiment Score')


