# end goal is to just be able to join a single object of secondary data to weekly stats to make mass dataframe
# currently am running three joins which seems redundant
# some snap counts are wrong, this is where I need to use SQL

# load initial data
games <- read_csv("./data/game_data.csv")
rosters <- read_csv("./data/rosters.csv")

# helper functions
scrape_page <- function(page) {
  read_csv(
    glue(
      "https://raw.githubusercontent.com/jchernak96/NFL-Injury-Data-PFR-/master/Data/PFR_{page}_Injuries.csv"
    )
  )
}

# clean injury/snap data ----
raw_injuries <-
  tibble(year = 2010:2020) %>%
  mutate(year_df = map(year, scrape_page)) %>%
  unnest(year_df)

names(raw_injuries) <- tolower(names(raw_injuries))
names(raw_injuries) <-
  gsub(
    x = names(raw_injuries),
    pattern = "\\.",
    replacement = "_"
  )

# distinct
cleaned_injuries <-
  subset(raw_injuries, !duplicated(subset(
    raw_injuries,
    select = c(gsis_id, season, week)
  ))) %>%
  select(
    gsis_id,
    season,
    week,
    team_abbr,
    active_inactive,
    game_designation,
    started,
    total_snaps,
    offense_snaps,
    offense_snap_rate,
    special_teams_snaps
  ) %>%
  filter(!is.na(gsis_id)) %>%
  rename(player_id = gsis_id)

# join game data ----
joined <- cleaned_injuries %>%
  left_join(games, by = c("season", "week", "team_abbr" = "team")) %>%
  filter(week <= 17) %>% 
  group_by(player_id, season, week) %>%
  mutate(
    started_bi = ifelse(started == "YES", 1, 0),
    active_bi = ifelse(active_inactive == "Active", 1, 0),
    healthy_bi = ifelse(game_designation == "Healthy", 1, 0),
    any_snaps_bi = ifelse(total_snaps >= 1, 1, 0),
    qual_snaps_bi = ifelse(offense_snap_rate >= .5, 1, 0)
  ) %>%
  ungroup()

# weekly data ----
weekly_data <- joined %>%
  rename(recent_team = team_abbr,
         opponent = opp) %>%
  unite(col = score, team_score, opp_score, sep = "-") %>% 
  select(
    player_id,
    season:offense_snap_rate,
    opponent,
    score,
    location,
    team_result:opp_coach,
    game_id
  )

# season data ----
season_averages <- joined %>%  
  group_by(player_id, season) %>% 
  filter(n() > 1) %>% 
  summarise(across(.cols = c(total_snaps:special_teams_snaps),
                   .fns = mean, na.rm = T)) %>% 
  rename(total_snaps_per_game = total_snaps,
         offense_snaps_per_game = offense_snaps,
         special_teams_snaps_per_game = special_teams_snaps) %>% 
  select(1:2, contains("snap"))

season_totals <- joined %>%  
  group_by(player_id, season) %>% 
  filter(n() > 1) %>% 
  summarise(across(.cols = c(total_snaps:special_teams_snaps, started_bi:last_col()),
                   .fns = sum),
            games = n()) %>% 
  mutate(start_rate = started_bi / games,
         active_rate = active_bi / games,
         healthy_rate = healthy_bi / games,
         qual_snap_rate = qual_snaps_bi / games) %>% 
  select(1:2, games, total_snaps, offense_snaps, special_teams_snaps, 
         started = started_bi,
         start_rate:qual_snap_rate)

season_data <- season_totals %>% left_join(season_averages, by = c("player_id", "season")) %>% 
  mutate(across(where(is.numeric), round, 2))

# export ----
write_csv(weekly_data, "./data_output/weekly_data.csv")
write_csv(season_data, "./data_output/season_data.csv")
