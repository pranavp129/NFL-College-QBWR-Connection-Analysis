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
library(grid)

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

# filter by duos with more than 50 nfl plays
both_levels_pairs_filtered <- both_levels_pairs %>% 
  filter(nfl_pbp_count >= 50, college_pbp_count > 10)

# filter out players that were drafted the same year
# if first_nfl_season is the year after last_college_season then both players were drafted at the same time
# Result: This only removes Haskins/McLaurin && Beathard/Kittle since they were drafted the same year
both_levels_pairs_filtered <- both_levels_pairs_filtered %>%
  filter((nfl_first_season - last_college_season) != 1)

# removing Kyler Murray and Christian Kirk duo since Kirk was on the team first
both_levels_pairs_filtered <- both_levels_pairs_filtered %>% 
  filter(!(passer == "K.Murray" & receiver == "C.Kirk"))

# removing Kyler Murray and Christian Kirk duo since Kirk was on the team first
both_levels_pairs_filtered <- both_levels_pairs_filtered %>% 
  filter(!(passer == "K.Murray" & receiver == "C.Kirk"))

# get QB epa by years
qb_epa_by_year <- nfl_pbp %>%
  filter(!is.na(passer_player_name), !is.na(qb_epa)) %>%
  group_by(passer_player_name, passer_player_id, season) %>%
  summarise(avg_qb_epa = mean(qb_epa, na.rm = TRUE), .groups = "drop")

# combine pair data with epa numbers
both_levels_with_epa <- both_levels_pairs_filtered %>%
  mutate(year_before = nfl_first_season - 1) %>%
  left_join(qb_epa_by_year, by = c("passer_id" = "passer_player_id", "year_before" = "season")) %>%
  rename(qb_epa_year_before = avg_qb_epa) %>%
  left_join(qb_epa_by_year, by = c("passer_id" = "passer_player_id", "nfl_first_season" = "season")) %>%
  rename(qb_epa_year_after = avg_qb_epa)

# find epa change
both_levels_with_epa <- both_levels_with_epa %>% 
  mutate(epa_change_with_wr = qb_epa_year_after - qb_epa_year_before)

# plot change in epa
ggplot(both_levels_with_epa, aes(x = qb_epa_year_before, xend = qb_epa_year_after, 
                                 y = reorder(passer, qb_epa_year_after), yend = reorder(passer, qb_epa_year_after))) +
  geom_segment(aes(color = receiver), size = 1.2, 
               arrow = arrow(type = "closed", length = unit(0.2, "inches")), color=both_levels_with_epa$team_color, show.legend = FALSE) +  # Remove legend
  geom_point(aes(x = qb_epa_year_before), shape = 21, fill = "black", size = 3) +
  geom_point(aes(x = qb_epa_year_after), shape = 21, fill = "black", size = 3) +
  geom_text(aes(x = (qb_epa_year_before + qb_epa_year_after) / 2, y = reorder(passer, qb_epa_year_after), 
                label = receiver), 
            size = 7, nudge_y = 0.2, color = both_levels_with_epa$team_color, show.legend = FALSE) +  # Remove legend
  geom_text(aes(x = qb_epa_year_before, y = reorder(passer, qb_epa_year_after), 
                label = as.character(year_before)), 
            size = 5, nudge_y = -0.2, color = "black") +  # Label for before season
  geom_text(aes(x = qb_epa_year_after, y = reorder(passer, qb_epa_year_after), 
                label = as.character(nfl_first_season)), 
            size = 5, nudge_y = -0.2, color = "black") +  # Label for after season
  labs(
    x = "Average QB EPA Per Play",
    y = "Quarterback",
    title = "Change in QB EPA Before and After Reuniting with College Teammate",
    caption = "Data: CFBFastR, NFLVerse\n**Pranav Pitchala**"
  ) +
  nfl_analytics_theme() +
  theme(
    axis.title.x = element_text(size = 20, margin = margin(t = 20)),  # Top margin pushes x-axis title down
    axis.title.y = element_text(size = 20, margin = margin(r = 20)),  # Right margin pushes y-axis title left
    axis.text.x = element_text(size = 12),    # Increase font size for x-axis labels
    axis.text.y = element_text(size = 15, aes(color = both_levels_with_epa$team_color)),    # Increase font size for y-axis labels
    plot.title = element_text(size = 25, face = "bold"),  # Increase font size for the plot title
    plot.subtitle = element_text(size = 14),  # Increase font size for the subtitle
    plot.caption = element_text(size = 10)    # Increase font size for the caption
  )

