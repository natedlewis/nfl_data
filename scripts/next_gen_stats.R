# load next gen stats
ngs <- list.files(pattern = "*ing.csv") %>% 
  map_df(~read_csv(.))

# clean ngs data
ngs_wkly <- ngs %>% 
  relocate(player_gsis_id, .before = season) %>% 
  rename(player_id = player_gsis_id,
         player_name = player_display_name,
         position = player_position,
         team = team_abbr) %>% 
  filter(season_type == "REG") %>% 
  select(player_id:team, -season_type, avg_time_to_throw:avg_air_yards_to_sticks, passer_rating, 
         completion_percentage, max_air_distance, avg_cushion, percent_share_of_intended_air_yards, 
         avg_yac:avg_time_to_los, expected_rush_yards, rush_pct_over_expected)

# filer to totals
ngs_ovr <- ngs_wkly %>% filter(week == 0) %>% select(-week)

