library(tidyverse)
library(rvest)

# Establish # of seasons scraping, and base URL
season <- seq(1988, 2018, by = 1)
urls <- data_frame(url = str_c("https://www.hockey-reference.com/leagues/NHL_",season,"_games.html"), 
                  season = season)

# Gets season schedule, so I have a data frame of the exact games I want to scrape
get_schedule <- function(season) {
  
  url <- str_c("https://www.hockey-reference.com/leagues/NHL_",season,"_games.html")
  
  schedule <- url %>%
    read_html() %>% 
    html_nodes("#games th a") %>% 
    html_attr("href") %>%
    str_c("http://www.hockey-reference.com", .) %>%
    as_tibble() %>%
    set_names("url") %>%
    mutate(season = season)
  
  return(schedule)
  
}

# Runs the above function for every season
link_list <- pmap_df(list(seq(1988, 2018, by = 1)), get_schedule)

# Function returns "scoring summaries" for each game
get_box_score <- function(row_num, my_data) {

  progress_bar$tick()$print()
  
  box_score_data <- my_data[row_num,] %>%
    pull(url) %>%
    read_html() %>% 
    html_node("#scoring") %>% 
    html_table() %>% 
    set_names("time", "team", "jumbled_data") %>%
    mutate(jumbled_data = str_replace_all(jumbled_data, "[\r\n\t]", "")) %>% 
    filter(!str_detect(time, "Period")) %>%
    mutate(game_strength = str_split(jumbled_data, "—", simplify = TRUE)[,1]) %>%
    mutate(game_strength = if_else(game_strength == jumbled_data, "EV", game_strength)) %>%
    mutate(players = str_split(jumbled_data, "—", simplify = TRUE, n = 2)[,2]) %>% 
    mutate(players = if_else(players == "", jumbled_data, players)) %>%
    mutate(goal = str_split(players, "\\(", simplify = TRUE, n = 2)[,1]) %>%
    mutate(primary_assist = str_split(players, "\\)", simplify = TRUE)[,2]) %>%
    mutate(primary_assist = str_split(primary_assist, " and", simplify = TRUE, n = 2)[,1]) %>%
    mutate(secondary_assist = str_split(players, " and", simplify = TRUE, n = 2)[,2]) %>%
    mutate_all(as.character) %>%
    mutate_all(str_trim) %>%
    select(-c(jumbled_data, players)) %>%
    as_tibble() %>%
    mutate(season = pull(my_data[row_num,], season))
  
  return(box_score_data)
  
}

# Catches warnings/errors, so function keeps running 
try_get_box_score <- function(row_num, my_data) {
  tryCatch(get_box_score(row_num, my_data), 
           error = function(e) {data_frame()},
           warning = function(w) {data_frame()})
}

# Sets progress bar
progress_bar <- link_list %>%
  tally() %>%
  progress_estimated(min_time = 0)

# Runs the function for each link
box_score_data <- map_df(1:nrow(link_list), try_get_box_score, my_data = link_list)

# Saves data to an ".rds" file
# saveRDS(box_score_data, "hockey_reference_box_score_data.rds")
