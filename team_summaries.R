# join game data to weekly stats to find fantasy points scored/allowed by team
joined <- reg_wkly %>% select(player_id:week, position, nw_fantasy_points) %>% left_join(games, by = c("week", "season", "recent_team" = "team"))

# nw fantasy points scored by season/position
fpts_scored <- joined %>% 
  group_by(recent_team, season, position) %>% 
  summarise(fpts_scored = sum(nw_fantasy_points)) %>% 
  ungroup()

# nw fantasy points allowed by season/position
fpts_allowed <- joined %>% 
  group_by(opp, season, position) %>% 
  summarise(fpts_allowed = sum(nw_fantasy_points)) %>% 
  ungroup()

# join scored and allowed objects
fpts_by_tm <- fpts_scored %>% left_join(fpts_allowed, by = c("season", "position", "recent_team" = "opp"))

# pivot wider to arrange 2020 data by postion
fpts_allowed_wider <- fpts_allowed %>% filter(season == 2020) %>%  pivot_wider(names_from = position, values_from = fpts_allowed)

# remove stale objects
rm(fpts_scored, fpts_allowed)

  