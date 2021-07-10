clean_games <- function(g) {
  g1 <- games %>%
    rename(
      team = away_team,
      team_score = away_score,
      opponent = home_team,
      opponent_score = home_score,
      team_rest = away_rest,
      opponent_rest = home_rest,
      team_moneyline = away_moneyline,
      opponent_moneyline = home_moneyline,
      team_spread_odds = away_spread_odds,
      opp_spread_odds = home_spread_odds,
      team_coach = away_coach,
      opponent_coach = home_coach
    ) %>%
    mutate(
      location = ifelse(location == "Home", "Away", location),
      result = -1 * result,
      spread_line = -1 * spread_line
    )
  
  g2 <- games %>%
    rename(
      team = home_team,
      team_score = home_score,
      opponent = away_team,
      opponent_score = away_score,
      team_rest = home_rest,
      opponent_rest = away_rest,
      team_moneyline = home_moneyline,
      opponent_moneyline = away_moneyline,
      team_spread_odds = home_spread_odds,
      opponent_spread_odds = away_spread_odds,
      team_coach = home_coach,
      opponent_coach = away_coach
    )
  
  g <- bind_rows(g1, g2) %>%
    mutate(team = case_when(
      team == "OAK" ~ "LV",
      team == "SD" ~ "LAC",
      team == "STL" ~ "LA",
      TRUE ~ team
    )) %>%
    mutate(opponent = case_when(
      opponent == "OAK" ~ "LV",
      opponent == "SD" ~ "LAC",
      opponent == "STL" ~ "LA",
      TRUE ~ opponent
    )) %>%
    arrange(gameday, gametime, old_game_id, location)
  return(g)
}

games <- read_csv("https://raw.githubusercontent.com/nflverse/nfldata/master/data/games.csv")

games_cleaned <- clean_games() %>%
  select(
    season,
    week,
    location,
    team,
    opponent,
    team_score,
    opponent_score,
    spread_line,
    team_coach,
    opponent_coach,
    game_id
  ) %>%
  unite(col = final_score, team_score, opponent_score, sep = "-")

write_csv(games_cleaned, "./data/games.csv")
