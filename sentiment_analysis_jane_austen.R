library(tidytext)
library(textdata)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)

janeaustenr::austen_books()                         #importing books written by jane austen
       #OR
library(janeaustenr)
austen_books()                                      #two cols : text and book

data = austen_books() %>%  group_by(book) %>%       #grouping by category : book (left side)
            mutate(line_no = row_number()) %>%      #line_no col will show that row which have data  
           unnest_tokens(word, text)  #un nesting text into word/tokens     
      
edit(data)

#we can only sentiment's attitude by comparing it with bing
data = data %>% filter(book=="Emma")   #extracting data of Emma book only
View(data)

bing = get_sentiments("bing")   #get pre defined sentiments of bing
View(bing)


emma_sentiment <- data %>%
  inner_join(bing) %>%     #inner_join() would join bing only by word; sentiment (poitive/negative)
  count(book="Emma",index = line_no %/% 80, sentiment) %>% #index is extracting each 80th line #count() will return count of book as n
  spread(sentiment,n) %>%  #it will spread sentiments and n is freq
  mutate(sentiment = positive - negative) #add a new col having score

View(emma_sentiment)

#plotting
  
ggplot(emma_sentiment, aes(x=index,y=sentiment))  + geom_bar(stat="identity") 
    #geom_bar will bins by default  but in this case data is not fitting with bins so we use identity

#optimizing plot
data %>% inner_join(bing) %>%  count(word,sentiment,sort = T)
  #joining data to get identification of p/n words then counting in desc order

data %>% filter(n>1) %>% mutate(n= ifelse(sentiment=='positive' , n,-n)) %>% 
        ggplot( aes(x=word , y=n)) + geom_col() + coord_flip() 






























