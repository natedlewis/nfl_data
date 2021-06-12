position_fan <-  reg_wkly %>% mutate(position_fan = case_when(position == 'TE'| position == 'WR' ~ 'WR/TE', TRUE ~ position)) %>% 
  select(game_id, player_id, position_fan)


# nw fantasy points scored by season/position
fpts_scored <- reg_wkly %>% 
  group_by(recent_team, season, position, position_fan) %>% 
  summarise(fpts_scored = sum(nw_fantasy_points)) %>% 
  ungroup()

# nw fantasy points allowed by season/position
fpts_allowed <- reg_wkly %>% 
  filter(!is.na(position)) %>% 
  select(-position) %>% 
  mutate(position_fan = case_when(position == 'TE'| position == 'WR' ~ 'WR/TE', TRUE ~ position))
  group_by(opp, season, position) %>% 
  summarise(fpts_allowed = sum(nw_fantasy_points),
            per = sum(nw_fantasy_points)/16,) %>% 
  ungroup()

fpts_allowed %>% 
  filter(season == 2020) %>% arrange(-per) %>% select(-season, -fpts_allowed)

fpts_allowed_wider
  pivot_wider(id_cols = opp, names_from = position, values_from = per) %>% 
  rbind()

# join scored and allowed objects
fpts_by_tm <- fpts_scored %>% 
  left_join(fpts_allowed, by = c("season", "position", "recent_team" = "opp"))

# pivot wider to arrange 2020 data by postion
fpts_allowed_wider <- fpts_allowed %>% filter(season == 2020) %>%  pivot_wider(names_from = position, values_from = fpts_allowed) %>% 
  select(-"NA")

# remove stale objects
rm(fpts_scored, fpts_allowed)

  