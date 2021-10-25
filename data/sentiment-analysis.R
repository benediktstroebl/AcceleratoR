library(syuzhet)
library(tidyverse)

news_articles_politicians <-
  read.csv("data/df_deu_politicians_articles.csv",
           encoding = "UTF-8") %>%
  mutate(url = str_trim(url)) %>%
  mutate(
    description = case_when(
      description == "" ~ NA_character_,
      TRUE ~ description,
    )
  ) %>%
  distinct(url, .keep_all = TRUE) %>%
  drop_na(description)


# regular sentiment score using get_sentiment() function and method of your choice
# please note that different methods may have different scales
syuzhet_vector <- tibble(
  sentiment_score = get_sentiment(
    news_articles_politicians %>%
      pull(description), 
    method="syuzhet")
  )

# see the first row of the vector
head(syuzhet_vector)
# see summary statistics of the vector
summary(syuzhet_vector)

ggplot(syuzhet_vector,
       aes(
         x = sentiment_score
       )) +
  geom_histogram(
    bins = 8
  )

