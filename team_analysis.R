# prepare data ----

# load libs
library(tidyverse)

# load season stats
season <- read_csv("season_stats.csv")

# load weekly stats
weekly <- read_csv("weekly_stats.csv")

# remove duplicates
season <- season[!duplicated(season[ , c("player_id","season", "recent_team")]),]
weekly <- weekly[!duplicated(weekly[ , c("player_id", "game_id")]),]

write_csv(season, "season_stats.csv")
write_csv(weekly, "weekly_stats.csv")

# mutate position variable to WR/TE for receievrs and create new positinal ranks
season <- season %>% 
  mutate(nw_position = case_when(position == 'TE'| position == 'WR' ~ 'WR/TE', TRUE ~ position)) %>% 
  arrange(-nw_fpts) %>% 
  dplyr::group_by(nw_position, season) %>%
  dplyr::mutate(nw_pos_rk = 1:n()) %>% 
  arrange(-fpts) %>% 
  mutate(std_pos_rk = 1:n()) %>% 
  ungroup()

# season stats  by position ----

# qb season stats
qb <- season %>% 
  filter(position == "QB", nw_pos_rk <= 25) %>% 
  select(season, player_name, position, recent_team, games,
         cmp:int_tds,
         car:rush_tds, in_five_ten,
         nw_fpts:std_pos_rk,
         player_id) %>% 
  select(season, nw_pos_rk, everything()) %>% 
  rename(rk = nw_pos_rk) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  arrange(-season, rk)

#rb season stats
rb <- season %>% 
  filter(position == "RB", nw_pos_rk <= 53) %>% 
  select(season, player_name, position, recent_team, games,
         car:combo_yds_150,
         rec:rec_tds, rz_tgts,
         nw_fpts:std_pos_rk,
         player_id) %>% 
  select(season, nw_pos_rk, everything()) %>% 
  rename(rk = nw_pos_rk,
         pct_stacked = percent_attempts_gte_eight_defenders) %>% 
  mutate(across(where(is.numeric), round, 2),
         diff = std_pos_rk - rk) %>% 
  arrange(-season, rk)

# wr/te stats
rec <- season %>% 
  filter(nw_position == "WR/TE", nw_pos_rk <= 56) %>% 
  select(season, player_name, position, recent_team, games,
         rec:cth_rate,
         cush:rec_td_50,
         nw_fpts:std_pos_rk,
         player_id) %>% 
  rename(rk = nw_pos_rk,
         std_rk = std_pos_rk,
         "100_yds" = rec_yds_100,
         "150_yds" = rec_yds_150,
         "200_yds" = rec_yds_200,
         "0_19" = rec_td_1,
         "20_49" = rec_td_20,
         "50+" = rec_td_50) %>% 
  mutate(diff = std_rk - rk,
         across(where(is.numeric), round, 2)) %>% 
  select(season, rk, everything()) %>% 
  arrange(-season, rk)

# weekly stats by position ----

# rb weekly stats
rb_wkly <- weekly %>% 
  filter(position == "RB") %>% 
  select(season, week, player_name, recent_team,
         carries:rushing_tds, rushing_first_downs,
         touches:neu_carries,
         rush_yds_100:rush_yds_200,
         rush_td_1:rush_td_50, rec_td_1:rec_td_50,
         total_snaps:offense_snap_rate,
         receptions:receiving_tds,
         rec_yds_100, combo_yds_100, combo_yds_150,
         fantasy_points, nw_fantasy_points,
         full_name, player_id) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  arrange(-season, week, -nw_fantasy_points)

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

# distribution/opportunity shares ----

shares <- weekly %>% 
  left_join(tm_wkly, by = c("week", "season", "recent_team", "position")) %>% 
  mutate(pct_touches = touches/tm_touches,
         usage_rate = touches/offense_snaps)

shares2 <- shares %>% 
  select(player_id:week, position, offense_snaps, usage_rate, touches, tm_touches, pct_touches) %>% 
  filter(season == 2020, position == "RB") %>% 
  mutate(across(where(is.numeric), round, 2))

shares_season <- shares2 %>% group_by(player_id, season) %>% 
  summarise(player_name = last(player_name),
            team = last(recent_team),
            games = n(),
            pct_touches = mean(pct_touches, na.rm = TRUE)) %>% 
  mutate(across(where(is.numeric), round, 2))

# nw fpts allowed by team/position ----

# nw fantasy points allowed by season/position
fpts_allowed <- weekly %>% 
  filter(!is.na(position)) %>% 
group_by(opp, season, position) %>% 
  summarise(fpts_allowed = sum(nw_fantasy_points),
            per = sum(nw_fantasy_points)/16,) %>% 
  ungroup()

# pivot wider to arrange 2020 data by postion
fpts_allowed_wider <- fpts_allowed %>% 
  filter(season == 2020) %>% select(-fpts_allowed) %>% 
  pivot_wider(names_from = position, values_from = per) %>% 
  mutate(across(where(is.numeric), round, 1))
