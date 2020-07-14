#lib
library(rtweet)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(echarts4r)

#cofigue proxy
file.edit('~/.Renviron')

#Acquire Tweets
token <- create_token(
  app = "lwlcomments",
  access_token = '2613114248-ejOyQdAE61XLPDWhpOsELXsOZNjX6P3pFz49tdY',
  access_secret = '6M5rcdlODm6xjdiiYCxedVg5Gn7v3pe5lSLfrDKrPmJQh',
  consumer_key = 'xbXrnqF9GSmnLvEL5YRU4gt1n',
  consumer_secret = 'LTjWlyeYk3dybLtpMhPvLZIzYLEjvG0GqgMTFvvrqfN0ZQHGqh',
  set_renv = TRUE
)
get_token()


q <- "LGBTQ"
result <- search_tweets(
  q,
  n = 3200,
  type = "recent",
  include_rts = TRUE,
  geocode = NULL,
  max_id = NULL,
  parse = TRUE,
  token = NULL,
)
attributes(result)
result



#Tidy

#Substract text
text_df <- tibble(text=result$text)



text_df <- text_df %>%
  unnest_tokens(word,text)



data("stop_words")
text_df <- text_df %>%
  anti_join(stop_words)


stopwd <- data.frame(word = c('t.co','https','http'))
text_df <- text_df %>%
  anti_join(stopwd)
#Remove non-english word
text_df <- text_df[which(!grepl("[^\x01-\x7F]+", text_df$word)),]

#Remove numb
text_df1 <- gsub("[[:digit:]]*", "", text_df$word)

#Remove short word
text_df1 <- text_df1[!nchar(text_df1) < 2]

#WordFreq
data.frame(word=text_df1) %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word,n)) +
  geom_col() + 
  xlab(NULL) +
  coord_flip()


#WordCloud
data.frame(word=text_df1) %>%
  count(word, sort = TRUE) %>%
  #filter(n > 100) %>%
  mutate(word = reorder(word,n)) %>%
  e_color_range(n, color) %>% 
  e_chart(word)%>%
  e_cloud(word,n,shape = "circle",color) %>%
  e_tooltip()

#Emotion 

#Happy word contained
nrcjoy <- get_sentiments('nrc') %>%
  filter(sentiment == 'joy')
  
data.frame(word=text_df1) %>%
  inner_join(nrcjoy) %>%
  count(word,sort = TRUE)


#
df <- data.frame(word=text_df1)

df <- df %>%
  inner_join(get_sentiments('bing')) %>%
  count(word, sentiment) %>%
  spread(sentiment,n,fill = 0) %>%
  mutate(sentiment = positive - negative)


df$extra <- df$sentiment
df$positive1 <- df$positive
df$negative1 <- df$negative



df$orit <- 'T'
df$orit[df$sentiment < 0]<- 'F'


df %>% 
  e_chart(word) %>%
  e_scatter(positive,positive1) %>%
  e_scatter(negative,negative1 ,x_index = 1, y_index = 1) %>%
  e_grid(width = "35%") %>%
  e_grid(width = "35%",left = "55%") %>%
  e_y_axis(gridIndex = 1) %>% # put x and y on grid index 1
  e_x_axis(gridIndex = 1) %>%
  e_tooltip()




df %>% 
  e_chart(word) %>%
  e_scatter(sentiment,extra) %>%
  e_tooltip() %>%
  e_brush()

df %>%
  e_color_range(sentiment, color) %>% 
  e_chart(word)%>%
  e_cloud(word,sentiment,shape = "circle",color, sizeRange = c(10, 50)) %>%
  e_tooltip()


library(gutenbergr)
tidy_books <- gutenberg_download(c(1260))
Jane <- tidy_books  %>%
  unnest_tokens(sentence,
                text,
                token="regex",
                pattern="Chapter|CHAPTER [\\dIVXLC]")%>%
  ungroup()

install.packages("sentimentr")
library(sentimentr)

mytext <- c(
  'do you like it?  But I hate really bad dogs',
  'I am the best friend.',
  'Do you really like it?  I\'m not a fan'
)

mytext <- get_sentences(mytext)
JaneMustHappy <- sentiment(mytext)



mytext <- Jane$sentence

JaneMustHappy$index <- row.names(JaneMustHappy)

JaneMustHappy %>%
  e_chart(index) %>%
  e_line(sentiment) %>%
  e_tooltip() %>%
  e_datazoom(type = "slider") 
