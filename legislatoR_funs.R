library(tidyverse)
library(legislatoR)

lsf.str("package:legislatoR")
cld_content("deu")

core_de <- get_core("deu")

political_de <- get_political("deu")

traffic_de <- get_traffic("deu")

ids_de <- get_ids("deu")

history_de <- get_history("deu")

social_de <- get_social("deu")

portrait_de <- get_portrait("deu")

office_de <- get_office("deu")

profession_de <- get_profession("deu")

# traffic
traffic_de %>% 
  group_by(pageid) %>% 
  summarise(
    min_date = min(date),
    max_date = max(date),
    sum_traffic = sum(traffic),
    date_range_years = difftime(max_date, min_date) %>% lubridate::time_length("years")
  ) %>% 
  left_join(core_de, by = "pageid") %>% 
  arrange(desc(sum_traffic))

office_de %>% 
  pivot_longer(
    cols = -wikidataid,
    names_to = "var",
    values_to = "value"
  ) %>% 
  # group_by(wikidateid) %>% 
  filter(value == TRUE) 

office_Q2492 <- office_de %>% 
  pivot_longer(
    cols = -wikidataid,
    names_to = "var",
    values_to = "value"
  ) %>% 
  # group_by(wikidateid) %>% 
  filter(value == TRUE) %>% 
  filter(wikidataid == "Q2492")

stop_words <- tidytext::stop_words %>% 
  distinct(word) %>% 
  pull

# non_stop_words_to_upper_first <- function(word) {
#   if(!word %in% stop_words) {
#     return(str_replace(word, "^.", toupper))
#   } else {
#     return(word)
#   }
# }

# custom_regex <- "Of|The"

# custom_regex <- stop_words 
  # toString %>% 
  # str_replace_all(", ", "|") %>% 
  
custom_regex <- str_c("\\b", stop_words %>% str_to_title, "\\b", collapse = "|")

stop_words_regex <- str_c("\\b", stop_words %>% str_to_title, "\\b", collapse = "|")


# non_stop_words_to_upper_first("about england")

# test office jobs
office_Q2492 %>% 
  mutate(
    var_new = var %>% 
      str_replace_all("_", " ") %>% 
      str_replace_all("[.]", " ") %>% 
      str_replace_all("((?<=[:space:])|^).", toupper) %>% 
      str_replace_all(custom_regex, tolower) %>% 
      str_replace_all("^.", toupper)
      # str_replace_all(
        # stop_words %>% toString %>% str_replace_all(", ", "|") %>% str_to_title,
        # stop_words %>% str_to_title %>% str_c(collapse = "|"),
        # str_to_lower
      )
      # str_replace_all(!var %in% stop_words,)
      # str_replace_all("\\w", non_stop_words_to_upper_first)
  

# str_test <- "Chairman of the cdu"

# non_stop_words_to_upper_first <- function(str) {
  # split_string <- str_split(str, " ") %>% unlist
  # 
# }

# # str_vec <- str_split(str_test, " ") %>% 
#   unlist

# str_vec


# stop_words %>% toString() %>% str_replace_all(", ", "|")

# for(i in str_vec) {
#   if(i %in% stop_words) {
#     print(str_replace("^.", toupper))
#   } else {
#     print(i)
#   }
# }

# str_purrred <- map_chr(str_vec, ~ str_replace_all("")) 
# str_purrred 

# 
# str_split(str_test, " ") %>% 
#   unlist %in% stop_words


# profession
df_profession <- profession_de %>% 
  pivot_longer(
    cols = -wikidataid,
    names_to = "var",
    values_to = "value"
  ) %>% 
  filter(value == TRUE) %>% 
  mutate(
    var_new = var %>% 
      str_replace_all("_", " ") %>% 
      str_replace_all("[.]", " ") %>% 
      str_replace_all("((?<=[:space:])|^).", toupper) %>% 
      str_replace_all(custom_regex, tolower) %>% 
      str_replace_all("^.", toupper)
  ) 

unique_professions <- df_profession %>% 
  distinct(var_new) %>% 
  pull

unique_professions
  
  
  
# social

