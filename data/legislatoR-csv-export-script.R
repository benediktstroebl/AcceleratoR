library(legislatoR)
library(tidyverse)
library(httr)

api_key <- "&apiKey=07288a2d35394938b113ad3cf504d9cd"

root_url <- "https://newsapi.org/v2/everything?q="

politican_name <- "Karl%20Lauterbach"

deu_politicians <- get_core(legislature = "deu")

write.csv(deu_politicians,"data/deu_politicians.csv", row.names = FALSE)
