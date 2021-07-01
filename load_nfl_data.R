# Helper functions ----
clean_games <- function(g) {
  g1 <- games %>%
    rename(
      team = away_team,
      team_score = away_score,
      opp = home_team,
      opp_score = home_score,
      team_rest = away_rest,
      opp_rest = home_rest,
      team_moneyline = away_moneyline,
      opp_moneyline = home_moneyline,
      team_spread_odds = away_spread_odds,
      opp_spread_odds = home_spread_odds,
      team_coach = away_coach,
      opp_coach = home_coach
    ) %>%
    mutate(
      location = ifelse(location  ==  "Home", "Away", location),
      result = -1 * result,
      spread_line = -1 * spread_line
    )
  
  g2 <- games %>%
    rename(
      team = home_team,
      team_score = home_score,
      opp = away_team,
      opp_score = away_score,
      team_rest = home_rest,
      opp_rest = away_rest,
      team_moneyline = home_moneyline,
      opp_moneyline = away_moneyline,
      team_spread_odds = home_spread_odds,
      opp_spread_odds = away_spread_odds,
      team_coach = home_coach,
      opp_coach = away_coach
    )
  
  g <- bind_rows(g1, g2) %>%
    mutate(team = case_when(
      team == 'OAK' ~ 'LV',
      team == 'SD' ~ 'LAC',
      team == 'STL' ~ 'LA',
      TRUE ~ team
    )) %>%
    mutate(opp = case_when(opp == 'OAK' ~ 'LV',
                           opp == 'SD' ~ 'LAC',
                           opp == 'STL' ~ 'LA',
                           TRUE ~ opp)) %>%
    arrange(gameday, gametime, old_game_id, location)
  return(g)
}
scrape_page <- function(page) {
  read_csv(
    glue(
      "https://raw.githubusercontent.com/jchernak96/NFL-Injury-Data-PFR-/master/Data/PFR_{page}_Injuries.csv"
    )
  )
}
# Rosters ----
# Load raw roster data
raw_rosters <- fast_scraper_roster(2010:2021) %>%
  mutate(nw_position = case_when(
    position == 'TE' | position == 'WR' ~ 'WR/TE',
    position == 'FB' ~ 'RB',
    TRUE ~ position
  )
)

# Load dynastyprocess player ids
raw_dp_rosters <-
  read_csv(
    "https://raw.githubusercontent.com/dynastyprocess/data/master/files/db_playerids.csv"
  ) %>%
  select(ends_with("_id", ignore.case = TRUE, vars = NULL),
         draft_year,
         draft_round,
         draft_pick) %>%
  mutate(sleeper_id = as.character(sleeper_id))

# Join roster data
rosters <- raw_rosters %>%
  left_join(raw_dp_rosters) %>%
  filter(!is.na(position)) %>%
  select(
    season,
    full_name,
    position,
    nw_position,
    birth_date,
    height,
    weight,
    college,
    draft_year,
    draft_round,
    draft_pick,
    status,
    ends_with("_id", vars = NULL))

# Games ----
# Load games
games <-
  read_csv("https://raw.githubusercontent.com/nflverse/nfldata/master/data/games.csv")

write_csv(games, "game_data.csv")

games <- clean_games(games) %>%
  select(
    game_id,
    season,
    week,
    team,
    team_score,
    opp,
    opp_score,
    location,
    spread_line,
    team_coach,
    opp_coach
  ) %>%
  mutate(team_result = team_score - opp_score) %>%
  relocate(team_result, .before = location)

# Injuries ----
# Load injuiry data
raw_injuries <-
  tibble(year = 2010:2020) %>%
  mutate(year_df = map(year, scrape_page)) %>%
  unnest(year_df)

names(raw_injuries) <- tolower(names(raw_injuries))
names(raw_injuries) <-
  gsub(x = names(raw_injuries),
       pattern = "\\.",
       replacement = "_")

# Only distinct
cleaned_injuries <-
  subset(raw_injuries,!duplicated(subset(
    raw_injuries, select = c(gsis_id, season, week)
  ))) %>%
  select(
    gsis_id,
    season,
    week,
    active_inactive,
    game_designation,
    started,
    total_snaps,
    offense_snaps,
    offense_snap_rate
  ) %>%
  filter(!is.na(gsis_id)) %>%
  rename(player_id = gsis_id)

# Export ----
write_csv(rosters, "rosters.csv")
write_csv(cleaned_injuries, "injury_data.csv")
write_csv(games, "games.csv")

