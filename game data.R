# load game data
games <- read_csv("https://raw.githubusercontent.com/nflverse/nfldata/master/data/games.csv")

# clean game data
g1 <- games %>% 
  rename(team = away_team,
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
         opp_coach = home_coach) %>% 
  mutate(location = ifelse(location  ==  "Home","Away",location),
         result = -1 * result,spread_line = -1 * spread_line)

g2 <- games %>% 
  rename(team = home_team,
         team_score = home_score,
         opp = away_team,
         opp_score = away_score,
         team_rest = home_rest,
         opp_rest = away_rest,
         team_moneyline = home_moneyline,
         opp_moneyline = away_moneyline,
         team_spread_odds = home_spread_odds,
         opp_spread_odds = away_spread_odds,           
         team_coach = home_coach,opp_coach = away_coach)

games <- bind_rows(g1,g2) %>% 
  arrange(gameday, gametime, old_game_id, location) %>% 
  mutate(
    team = case_when(
      team == 'OAK' ~ 'LV',
      team == 'SD' ~ 'LAC',
      team == 'STL' ~ 'LA',
      TRUE ~ team)) %>% 
  mutate(
    opp = case_when(
      opp == 'OAK' ~ 'LV',
      opp == 'SD' ~ 'LAC',
      opp == 'STL' ~ 'LA',
      TRUE ~ opp))

rm(g1, g2)
