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
  filter(!is.na(passer_player_name), !is.na(receiver_player_name), completion == 1, penalty_flag == FALSE) %>%
  select(season = year, college_team = pos_team, passer = passer_player_name, receiver = receiver_player_name) %>%
  group_by(passer, receiver) %>%
  summarise(
    last_college_season = max(season),
    cfb_catches = n(),
    .groups = "drop"
  )

# get nfl pairs
nfl_pbp <- nflreadr::load_pbp(2013:2024)
colnames(nfl_pbp)

nfl_pairs <- nfl_pbp %>% 
  filter(!is.na(passer_player_name), !is.na(receiver_player_name), complete_pass == 1, penalty == 0) %>%
  select(season, nfl_team = posteam, passer = passer, passer_id = passer_player_id, receiver = receiver_player_name, receiver_id = receiver_id) %>%
  group_by(passer, passer_id, receiver, receiver_id) %>%
  summarise(
    nfl_first_season = min(season),
    nfl_catches = n(),
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

# remove Taulia Tagovailoa from data
combined_labels <- combined_labels %>%
  filter(!(cfb_catches == 3 & passer == "T.Tagovailoa"))

ggplot(data = combined_labels, aes(x = cfb_catches, y = nfl_catches)) +
  geom_point(shape = 21, size = 4, fill = "blue") +  # Increased size of points
  geom_text_repel(aes(label = duo_name), size = 5, box.padding = 0.35, point.padding = 0.5, max.overlaps = 10) +  # Increased text size and added padding
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6),
                     labels = scales::label_comma()) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6),
                     labels = scales::label_comma()) +
  labs(x = "College Catches between Duo",
       y = "NFL Catches between Duo",
       title = "Catches between Teammates that played together in CFB and NFL",
       subtitle = "College vs. NFL",
       caption = "Data: CFBFastR, NFLVerse\n**Pranav Pitchala**") +
  nfl_analytics_theme() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Adjusted title size
    plot.subtitle = element_text(size = 14, hjust = 0.5),  # Adjusted subtitle size
    axis.title = element_text(size = 14),  # Adjusted axis titles size
    axis.text = element_text(size = 12),  # Adjusted axis text size
  )

# filter by duos with more than 50 nfl plays
both_levels_pairs_filtered <- both_levels_pairs %>% 
  filter(nfl_catches >= 55, cfb_catches > 10)

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
  rename(qb_epa_with_wr = avg_qb_epa)

# find epa change
both_levels_with_epa <- both_levels_with_epa %>% 
  mutate(epa_change_with_wr = qb_epa_with_wr - qb_epa_year_before)

# plot change in epa
avg_change <- round(mean(both_levels_with_epa$epa_change_with_wr, na.rm = TRUE), 3)

ggplot(both_levels_with_epa, aes(x = qb_epa_year_before, xend = qb_epa_with_wr, 
                                 y = reorder(passer, qb_epa_with_wr), yend = reorder(passer, qb_epa_with_wr))) +
  geom_segment(size = 1.2, 
               arrow = arrow(type = "closed", length = unit(0.2, "inches")), color=both_levels_with_epa$team_color, show.legend = FALSE) +  # Remove legend
  geom_point(aes(x = qb_epa_year_before), shape = 21, fill = "black", size = 3) +
  geom_point(aes(x = qb_epa_with_wr), shape = 21, fill = "black", size = 3) +
  geom_text(aes(x = (qb_epa_year_before + qb_epa_with_wr) / 2, y = reorder(passer, qb_epa_with_wr), 
                label = receiver), 
            size = 7, nudge_y = 0.3, color = both_levels_with_epa$team_color, show.legend = FALSE) +  # Remove legend
  geom_text(aes(x = qb_epa_year_before, y = reorder(passer, qb_epa_with_wr), 
                label = as.character(year_before)), 
            size = 5, nudge_y = -0.2, color = "black") +  # Label for before season
  geom_text(aes(x = qb_epa_with_wr, y = reorder(passer, qb_epa_with_wr), 
                label = as.character(nfl_first_season)), 
            size = 5, nudge_y = -0.2, color = "black") +  # Label for after season
  labs(
    x = "Average QB EPA Per Play",
    y = "Quarterback",
    title = "Change in QB EPA Before and After\nReuniting with College Teammate",
    subtitle = paste0("Average change in EPA per play with WR: ", avg_change),
    caption = "Data: CFBFastR, NFLVerse\n**Pranav Pitchala**"
  ) +
  nfl_analytics_theme() +
  theme(
    axis.title.x = element_text(size = 20, margin = margin(t = 20)),  # Top margin pushes x-axis title down
    axis.title.y = element_text(size = 20, margin = margin(r = 20)),  # Right margin pushes y-axis title left
    axis.text.x = element_text(size = 12),    # Increase font size for x-axis labels
    axis.text.y = element_text(size = 15, color = "black"),    # Increase font size for y-axis labels
    plot.title = element_text(size = 25, face = "bold", margin = margin(b = 10)),  # Increase font size for the plot title
    plot.subtitle = element_text(size = 14, color = "red"),  # Increase font size for the subtitle
    plot.caption = element_text(size = 10)    # Increase font size for the caption
  ) 

# find avg change when excluding Murray / Brown negative number
no_kyler <- both_levels_with_epa %>% 
  filter(!(passer == "K.Murray"))

alt_average_change <- round(mean(no_kyler$epa_change_with_wr, na.rm = TRUE), 3)

# Let's compare this to how first round receivers affect their quarterbacks in general

# Load draft data
draft_picks <- load_draft_picks()

# Get first round WRs since 2014
first_round_wrs <- draft_picks %>%
  filter(position == "WR", round == 1, season >= 2014) %>%
  select(receiver_gsis = gsis_id, receiver = pfr_player_name, rookie_season = season, team = team)

# Get QB's and their draft year
qb_draft_info <- draft_picks %>%
  filter(position == "QB") %>%
  select(passer_gsis = gsis_id, passer = pfr_player_name, rookie_season = season, team = team)

# match rookie wr's and qb's
rookie_wr_qbs <- nfl_pbp %>%
  filter(!is.na(receiver_id), !is.na(passer_id), complete_pass == 1, penalty == 0) %>%
  inner_join(first_round_wrs, by = c("receiver_id" = "receiver_gsis")) %>%
  filter(season == rookie_season) %>%
  group_by(receiver_id, receiver_player_name, rookie_season, passer_id, passer_player_name) %>%
  summarise(nfl_catches = n(), 
            nfl_team = first(team),
            .groups = "drop")

# calculate before and after epa's
rookie_wr_qbs_epa <- rookie_wr_qbs %>%
  mutate(year_before = rookie_season - 1) %>%
  left_join(qb_epa_by_year, by = c("passer_id" = "passer_player_id", "year_before" = "season")) %>%
  rename(qb_epa_before = avg_qb_epa) %>%
  left_join(qb_epa_by_year, by = c("passer_id" = "passer_player_id", "rookie_season" = "season")) %>%
  rename(qb_epa_with_wr = avg_qb_epa) %>%
  mutate(epa_change_with_wr = qb_epa_with_wr - qb_epa_before)

# adjust team abbr's
# Define abbreviation fixes (if needed)
abbr_fixes <- c(
  "SFO" = "SF",
  "TAM" = "TB",
  "OAK"  = "LV",
  "NWE" = "NE",
  "LVR"  = "LV",
  "NOR" = "NO",
  "KAN" = "KC"
)

# Replace abbreviations in rookie_wr_qbs_epa
rookie_wr_qbs_epa <- rookie_wr_qbs_epa %>%
  mutate(nfl_team = ifelse(nfl_team %in% names(abbr_fixes), abbr_fixes[nfl_team], nfl_team))


# add team color info
rookie_wr_qbs_epa <- rookie_wr_qbs_epa %>% 
  left_join(teams, by = c("nfl_team" = "team_abbr"))

# filter by number of catches
rookie_wr_qbs_epa <- rookie_wr_qbs_epa %>% 
  filter(nfl_catches >= 10) %>% 
  drop_na()

# removing backup quarterbacks to clear up space on chart and get a better idea of how this affects franchise qbs
exclude_qbs <- c("J.Brissett", "B.Osweiler", "J.Flacco", "D.Lock", "M.Jones", "A.Dalton", "J.McCown", "K.Orton", "C.Wentz", "T.Heinicke", "J.Dobbs", "R.Griffin", "E.Stick", "T.Devito")

rookie_wr_qbs_epa <- rookie_wr_qbs_epa %>%
  filter(!(passer_player_name %in% exclude_qbs))

# add Trevor Lawrence and Travis Etienne manually:
tlaw_etn_data <- data.frame(
  receiver_id = "	00-0036973",
  receiver_player_name = "T.Etienne",
  rookie_season = 2022,
  passer_id = "00-0036971",
  passer_player_name.x = "T.Lawrence",
  nfl_catches = 110,
  nfl_team = "JAX",
  year_before = 2021,
  passer_player_name.y = "T.Lawrence",
  qb_epa_before = -0.09506707,
  passer_player_name = "T.Lawrence",
  qb_epa_with_wr = 0.10875618,
  epa_change_with_wr = 0.20382325,
  team_color = "#006778",
  team_color2 = "#000000",
  stringsAsFactors = FALSE
)

rookie_wr_qbs_epa <- bind_rows(rookie_wr_qbs_epa, tlaw_etn_data)

# plot change in epa
avg_change <- round(mean(rookie_wr_qbs_epa$epa_change_with_wr, na.rm = TRUE), 3)

ggplot(rookie_wr_qbs_epa, aes(x = qb_epa_before, xend = qb_epa_with_wr, 
                              y = reorder(passer_player_name, qb_epa_with_wr), yend = reorder(passer_player_name, qb_epa_with_wr))) +
  geom_segment(size = 1, 
               arrow = arrow(type = "closed", length = unit(0.2, "inches")), color=rookie_wr_qbs_epa$team_color, show.legend = FALSE) +  
  geom_point(aes(x = qb_epa_before), shape = 21, fill = "black", size = 3) +
  geom_point(aes(x = qb_epa_with_wr), shape = 21, fill = "black", size = 3) +
  geom_text(aes(x = (qb_epa_before), y = reorder(passer_player_name, qb_epa_with_wr), 
                label = receiver_player_name), 
            size = 4, 
            nudge_x = 
              ifelse(rookie_wr_qbs_epa$epa_change_with_wr > 0,
                     -0.009,
                     0.01), 
            nudge_y = 0.35, 
            color = rookie_wr_qbs_epa$team_color, show.legend = FALSE) +  # Remove legend
  geom_text(aes(x = qb_epa_before, y = reorder(passer_player_name, qb_epa_with_wr), 
                label = as.character(year_before)), 
            size = 3, 
            nudge_x = -0.01, 
            nudge_y = -0.3, 
            color = "black") +  # Label for before season
  geom_text(aes(x = qb_epa_with_wr, y = reorder(passer_player_name, qb_epa_with_wr), 
                label = as.character(rookie_season)), 
            size = 3, 
            nudge_x = 0.01,
            nudge_y = -0.3, 
            color = "black") +  # Label for after season
  labs(
    x = "Average QB EPA Per Play",
    y = "Quarterback",
    title = "Change in QB EPA Before and After\nDrafting a First Round Reciever",
    subtitle = paste0("Average change in EPA per play with WR: ", avg_change),
    caption = "Data: CFBFastR, NFLVerse\n**Pranav Pitchala**"
  ) +
  nfl_analytics_theme() +
  theme(
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),  # Top margin pushes x-axis title down
    axis.title.y = element_text(size = 15, margin = margin(r = 20)),  # Right margin pushes y-axis title left
    axis.text.x = element_text(size = 12),    # Increase font size for x-axis labels
    axis.text.y = element_text(size = 12, color = "black"),    # Increase font size for y-axis labels
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 10)),  # Increase font size for the plot title
    plot.subtitle = element_text(size = 12, color = "red"),  # Increase font size for the subtitle
    plot.caption = element_text(size = 10)    # Increase font size for the caption
  ) 
