# download cfb and nfl data
# remotes::install_github("sportsdataverse/cfbfastR")
# install.packages("nflverse")

# start libraries and load theme
library(tictoc)
library(cfbfastR)
library(tidyverse)
library(nflverse)
library(dplyr)
library(stringr)
library(ggrepel)
source("GitHub/College-Connection-Analysis/QB_WR_College_Connection/theme.R")

# get college pbp data
tictoc::tic()
cfb_pbp <- data.frame()
seasons <- 2014:cfbfastR:::most_recent_cfb_season()
progressr::with_progress({
  cfb_pbp <- cfbfastR::load_cfb_pbp(seasons)
})
tictoc::toc()

# exploring CFB data
colnames(cfb_pbp)

cfb_pbp %>% 
  filter(passer_player_name == "C.J. Stroud")

stroud_test <- cfb_pbp %>% 
  filter(passer_player_name == "C.J. Stroud" & !is.na(receiver_player_name)) %>% 
  select(passer_player_name, receiver_player_name) %>% 
  count(passer_player_name, receiver_player_name) %>%
  rename(college_pbp_count = n)

# get cfb qb-wr pairs
college_pairs <- cfb_pbp %>%
  filter(!is.na(passer_player_name), !is.na(receiver_player_name)) %>%
  select(season = year, college_team = pos_team, passer = passer_player_name, receiver = receiver_player_name) %>%
  group_by(passer, receiver) %>%
  summarise(
    last_college_season = max(season),
    college_pbp_count = n(),
    .groups = "drop"
  )

# get nfl pairs
nfl_pbp <- nflreadr::load_pbp(2013:2024)
colnames(nfl_pbp)

nfl_pairs <- nfl_pbp %>% 
  filter(!is.na(passer_player_name), !is.na(receiver_player_name), complete_pass == 1) %>%
  select(season, nfl_team = posteam, passer = passer, passer_id = passer_player_id, receiver = receiver_player_name, receiver_id = receiver_id) %>%
  group_by(passer, passer_id, receiver, receiver_id) %>%
  summarise(
    nfl_first_season = min(season),
    nfl_pbp_count = n(),
    nfl_team = first(nfl_team),
    .groups = "drop"
  )

# get nfl team colors and merge data
teams <- nflreadr::load_teams(current = TRUE) %>%
  select(team_abbr, team_color, team_color2)

nfl_pairs <- nfl_pairs %>% 
  left_join(teams, by = c("nfl_team" = "team_abbr"))

# edit CFB names to match NFL formats
clean_name <- function(name) {
  name %>%
    # Remove suffixes (e.g., Jr., III)
    str_remove("\\s+(Jr\\.|II|III|IV)$") %>%
    # Extract first initial and last name (after the last space)
    str_replace("^([A-Za-z\\'])(?:[A-Za-z\\'\\.\\-]+\\s+)*([A-Za-z\\-]+)$", "\\1.\\2")
}

college_pairs <- college_pairs %>%
  mutate(
    passer = clean_name(passer),
    receiver = clean_name(receiver)
  )

# find with QB / WR duos played together at both levels
both_levels_pairs <- 
  inner_join(college_pairs, nfl_pairs, by = c("passer", "receiver"))

# plot college vs nfl snaps
combined_labels <- both_levels_pairs %>% 
  mutate(duo_name = str_c(passer, " & ", receiver))

ggplot(data = combined_labels, aes(x = college_pbp_count, y = nfl_pbp_count)) +
  geom_point(shape = 21) +
  geom_text_repel(aes(label = duo_name)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6),
                     labels = scales::label_comma()) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6),
                     labels = scales::label_comma()) +
  labs(x = "College Plays with duo involved",
       y = "NFL Plays with duo involved",
       title = "Plays with players that played together in CFB and NFL",
       subtitle = "College vs. NFL",
       caption = "Data: CFBFastR, NFLVerse
       **Pranav Pitchala**") +
  nfl_analytics_theme()
