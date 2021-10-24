library(tidyverse)
library(legislatoR)

parlspeech_de <- readRDS("Corp_Bundestag_V2.rds")

class(parlspeech_de)

# checks
parlspeech_de %>% 
  distinct(parliament)

(test_name_id <- parlspeech_de %>%
  mutate(
    parlspeech_id = paste0(
      speaker, "_", party
    )
  ) %>% 
  distinct(parlspeech_id))

# parlspeech_de %>%
#   mutate(
#     parlspeech_id = paste0(
#       speaker, "_", party
#     )
#   ) %>% 
#   distinct(parlspeech_id) 

parlspeech_de %>% 
  distinct(speaker) %>% 
  nrow

parlspeech_de %>% 
  distinct(party)

# number of speeches
parlspeech_de %>% 
  # group_by(speaker, party) %>% 
  group_by(speaker) %>% 
  # count %>% 
  summarise(
    n_speech = n(),
    min_date_speech = min(date, na.rm = T),
    max_date_speech = max(date, na.rm = T),
    mean_terms_speech = mean(terms, na.rm = T)
  ) %>% 
  ungroup %>% 
  arrange(desc(n_speech))

parlspeech_de_metrics <- parlspeech_de %>% 
  mutate(
    parlspeech_id = paste0(
      speaker, "_", party
    )
  ) %>% 
  filter(!is.na(party)) %>% 
  group_by(parlspeech_id) %>% 
  summarise(
    n_speech = n(),
    min_date_speech = min(date, na.rm = T),
    max_date_speech = max(date, na.rm = T),
    mean_terms_speech = mean(terms, na.rm = T)
  ) %>% 
  ungroup() 

parlspeech_de_metrics %>% 
  arrange(desc(n_speech))
  
# legislatoR --------------------------------------------------------------
core_de <- get_core("deu")
ids_de <- get_ids("deu")
political_de <- get_political("deu")

party_parlspeech <- political_de %>% 
  distinct(party) %>% 
  mutate(
    party_parlspeech = case_when(
      party %in% c("CDU", "CSU") ~ "CDU/CSU",
      party %in% c("PDS", "DIE LINKE") ~ "PDS/LINKE",
      party == "SPD" ~ "SPD",
      party == "FDP" ~ "FDP",
      party == "BÜNDNIS 90/DIE GRÜNEN" ~ "GRUENE",
      party == "AfD" ~ "AfD",
      TRUE ~ NA_character_
    )
  )

party_parlspeech

ids_de %>% 
  distinct(parlspeech) %>% 
  nrow

parlspeech_id_df <- political_de %>% 
  left_join(core_de) %>% 
  left_join(party_parlspeech) %>% 
  left_join(ids_de) %>% 
  ungroup() %>% 
  mutate(
    parlspeech_id = if_else(
      !is.na(parlspeech),
      paste0(parlspeech, "_", party_parlspeech),
      NA_character_
    )
  ) %>% 
  filter(!is.na(parlspeech_id)) %>% 
  distinct(parlspeech_id, pageid, wikidataid)

parlspeech_data_de <- parlspeech_de_metrics %>% 
  left_join(parlspeech_id_df)

# save data as csv
readr::write_csv(
  parlspeech_data_de,
  "parlspeech_data_de.csv"
)

rm(list = ls())



