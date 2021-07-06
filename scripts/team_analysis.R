  # load season stats
overall <- read_csv("./data/season_stats.csv")
weekly <- read_csv("./data/weekly_stats.csv")

# team stats ----

# weekly team totals by position
tm_pos_wkly <- weekly %>%
  filter(!is.na(position)) %>% 
  group_by(recent_team, week, season, position) %>% 
  summarise(coach = last(team_coach),
            tm_touches = sum(touches),
            tm_carries = sum(carries),
            tm_in_five_carries = sum(in_five_carries, na.rm = TRUE),
            tm_in_ten_carries = sum(in_ten_carries, na.rm = TRUE),
            tm_neu_carries = sum(neu_carries, na.rm = TRUE),
            tm_tgts = sum(targets, na.rm = TRUE),
            tm_rz_tgts = sum(rz_tgts, na.rm = TRUE),
            tm_ez_tgts = sum(ez_tgts, na.rm = TRUE),
            tm_deep_tgts = sum(deep_tgts, na.rm = TRUE),
            tm_nw_fpts = sum(nw_fantasy_points)) %>% 
  filter(!is.na(position)) %>% 
  arrange(-season, week, recent_team)

# weekly team totals
tm_tot_wkly <- weekly %>%
  group_by(recent_team, week, season) %>% 
  summarise(position = "TOT",
            coach = last(team_coach),
            tm_touches = sum(touches),
            tm_carries = sum(carries),
            tm_in_five_carries = sum(in_five_carries, na.rm = TRUE),
            tm_in_ten_carries = sum(in_ten_carries, na.rm = TRUE),
            tm_neu_carries = sum(neu_carries, na.rm = TRUE),
            tm_tgts = sum(targets, na.rm = TRUE),
            tm_rz_tgts = sum(rz_tgts, na.rm = TRUE),
            tm_ez_tgts = sum(ez_tgts, na.rm = TRUE),
            tm_deep_tgts = sum(deep_tgts, na.rm = TRUE),
            tm_nw_fpts = sum(nw_fantasy_points)) %>% 
  arrange(-season, week, recent_team)

# bind to make single weekly team totals object
tm_wkly <- tm_pos_wkly %>% bind_rows(tm_tot_wkly) %>% 
  arrange(-season, week, recent_team)

# sum weekly totals to get season totals
tm_season <- tm_wkly %>% 
  group_by(recent_team, season, position) %>% 
  summarise(coach = last(coach),
            tm_touches = sum(tm_touches),
            tm_carries = sum(tm_carries),
            tm_in_five_carries = sum(tm_in_five_carries, na.rm = TRUE),
            tm_in_ten_carries = sum(tm_in_ten_carries, na.rm = TRUE),
            tm_neu_carries = sum(tm_neu_carries, na.rm = TRUE),
            tm_tgts = sum(tm_tgts, na.rm = TRUE),
            tm_rz_tgts = sum(tm_rz_tgts, na.rm = TRUE),
            tm_ez_tgts = sum(tm_ez_tgts, na.rm = TRUE),
            tm_deep_tgts = sum(tm_deep_tgts, na.rm = TRUE),
            tm_nw_fpts = sum(tm_nw_fpts)) %>%
  arrange(-season, recent_team)

# nw fantasy points allowed by season/position
fpts_allowed <- weekly %>% 
  filter(!is.na(position)) %>% 
group_by(opponent, season, position) %>% 
  summarise(fpts_allowed = sum(nw_pts),
            per = sum(nw_pts)/16,) %>% 
  ungroup()

# pivot wider to arrange 2020 data by postion
fpts_allowed_wider <- fpts_allowed %>% 
  filter(season == 2020) %>% select(-fpts_allowed) %>% 
  pivot_wider(names_from = position, values_from = per) %>% 
  mutate(across(where(is.numeric), round, 1))
