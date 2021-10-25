library(tidyverse)
library(legislatoR)
library(lubridate)

# legislatoR data
core_de <- get_core("deu")
political_de <- get_political("deu")
traffic_de <- get_traffic("deu")

# radar coordination function
coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}


# test metrics ------------------------------------------------------------

# traffic per MP
traffic_per_mp_de <- traffic_de %>%
  group_by(pageid) %>% 
  summarise(
    min_date_traffic = min(date),
    max_date_traffic = max(date),
    sum_traffic = sum(traffic),
    date_range_years_traffic = difftime(max_date_traffic, min_date_traffic) %>% lubridate::time_length("years")
  ) %>% 
  ungroup

# sessions per MP
sessions_per_mp_de <- political_de %>% 
  group_by(pageid) %>% 
  summarise(session_n = n()) %>% 
  ungroup

# entry age first session
entry_age_first_session_de <- political_de %>% 
  left_join(core_de) %>% 
  group_by(pageid) %>% 
  filter(session == min(session)) %>% 
  ungroup() %>% 
  mutate(
    age_session_start = if_else(
      session_start > birth,
      time_length(difftime(session_start, birth), "years") %>% round(3),
      NA_real_
    )
  ) %>% 
  select(pageid, wikidataid, age_session_start) %>% 
  distinct(pageid, .keep_all = T)

## check uniqueness
entry_age_first_session_de %>% 
  group_by(pageid) %>% 
  mutate(n = n()) %>% 
  filter(n > 1)

# age at death
age_mp_de <- core_de %>% 
  mutate(
    age = case_when(
      death > birth ~ time_length(difftime(death, birth), "years") %>% round(3),
      is.na(death) ~ time_length(difftime(Sys.Date(), birth), "years") %>% round(3),
      TRUE ~ NA_real_
    )
  ) %>% 
  select(pageid, wikidataid, age)

# parlspeech metrics
parlspeech_metrics_de <- read_csv("parlspeech_data_de.csv") %>% 
  filter(!is.na(pageid)) %>% 
  mutate(pageid = pageid %>% as.character)






# final df ----------------------------------------------------------------

# rescale variables!
normalize <- function(x) {
  if (is.numeric(x)) {
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  } else {
    print("Vector is not numeric")
  }
}

df_metrics_de <- core_de %>% 
  distinct(pageid) %>% 
  left_join(traffic_per_mp_de) %>% 
  left_join(sessions_per_mp_de) %>% 
  left_join(entry_age_first_session_de) %>% 
  left_join(age_mp_de) %>% 
  left_join(parlspeech_metrics_de)

df_metrics_de_parlspeech <- df_metrics_de %>% 
  filter(!is.na(parlspeech_id)) 

df_metrics_de_parlspeech %>% 
  arrange(desc(mean_terms_speech)) %>% 
  slice(1:4) %>% 
  pull(parlspeech_id) -> top_4_speech_length

df_metrics_de_parlspeech %>% 
  filter(parlspeech_id %in% top_4_speech_length) %>% 
  select(pageid, parlspeech_id, everything()) %>% 
  select(
    # ids
    pageid, 
    parlspeech_id, 
    # metrics
    session_n,
    n_speech,
    sum_traffic,
    age,
    age_session_start,
    mean_terms_speech
  ) %>% 
  mutate(
    across(where(is.numeric), ~ normalize(.x), .names = "{col}_normalized")
  ) %>% 
  select(pageid, parlspeech_id, ends_with("normalized")) %>% 
  pivot_longer(
    cols = -c(pageid, parlspeech_id),
    names_to = "var",
    values_to = "value"
  ) %>% 
  # right order
  arrange(var) %>% 
  ggplot(
    aes(
      x = var,
      y = value,
      group = parlspeech_id,
      color = parlspeech_id,
      fill = parlspeech_id,
    )
  ) + 
  geom_point(show.legend = FALSE) +
  geom_polygon(alpha = 0.5) +
  coord_radar() +
  labs(x = "", y = "") +
  theme_bw() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  ) 


















# number of twitter follower
cranlogs::cran_downloads(
  packages = c("twitteR", "rtweet"), 
  from = Sys.Date() - 1500,
  to = Sys.Date()) %>% 
  group_by(package) %>% 
  summarise(sum_downloads = sum(count))



# library(rtweet)
# library(twitteR)
# 
# g
# get_followers(
#   "gregorgysi",
#   n = Inf,
#   retryonratelimit = T
#   # n = 1e10
# )

# rstats_tweets <- search_tweets(q = "#rstats",
#                                n = 500)
# 
# get_followers("gregorgysi")
# 
# # xpath_follower <- '//*[contains(concat( " ", @class, " " ), concat( " ", "r-1w6e6rj", " " ))]'
# xpath_follower <- '//*[@id="react-root"]/div/div/div[2]/main/div/div/div/div/div/div[2]/div/div/div[1]/div/div[5]/div[2]/a/span[1]/span'
# 
# xpath_follower <- "/html/body/div/div/div/div[2]/main/div/div/div/div/div/div[2]/div/div/div[1]/div/div[5]/div[2]/a/span[1]/span"
# 
# library(rvest)
# read_html("https://twitter.com/gregorgysi") %>% 
#   html_nodes("body") %>% 
#   xml2::xml_find_all(xpath = xpath_follower)
