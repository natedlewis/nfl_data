nw_rush_wkly <- data %>% 
  filter(.data$play_type %in% c("qb_kneel", "run")) %>% 
  dplyr::group_by(.data$rusher_player_id, .data$season, .data$week) %>%
  dplyr::summarize(
    player_name = dplyr::first(.data$rusher_player_name),
    recent_team = dplyr::first(.data$posteam),
    carries = dplyr::n(),
    rush_yds = sum(yards_gained),
    "rush_td_1" = sum(.data$td_player_id == .data$rusher_player_id & .data$yards_gained <= 9, na.rm = TRUE),
    "rush_td_10" = sum(.data$td_player_id == .data$rusher_player_id & .data$yards_gained >= 10 & yards_gained <= 29, na.rm = TRUE),
    "rush_td_30" = sum(.data$td_player_id == .data$rusher_player_id & .data$yards_gained >= 30 & yards_gained <= 49, na.rm = TRUE),
    "rush_td_50" = sum(.data$td_player_id == .data$rusher_player_id & .data$yards_gained >= 50, na.rm = TRUE),
    "rush_yds_100" = sum(ifelse(rush_yds >= 100 & rush_yds < 150, 1, 0)),
    "rush_yds_150" = sum(ifelse(rush_yds >= 150 & rush_yds < 200, 1, 0)),
    "rush_yds_200" = sum(ifelse(rush_yds >= 200, 1, 0)),
    "nw_fpts_rush" = sum(10 * rush_td_1,
                    15 * rush_td_10,
                    20 * rush_td_30,
                    25 * rush_td_50,
                    10 * rush_yds_100,
                    15 * rush_yds_150,
                    25 * rush_yds_200)) %>% 
  rename(player_id = rusher_player_id) %>% 
  dplyr::filter(!is.na(.data$player_id)) %>% 
  ungroup() %>% 
  left_join(roster %>% select(gsis_id, season, position, full_name), by = c("player_id" = "gsis_id", "season")) %>% 
  mutate(player_name = if_else(is.na(full_name), full_name, player_name),
         nw_position = case_when(position == 'TE'| position == 'WR' ~ 'WR/TE', TRUE ~ position)) %>% 
  select(player_id:recent_team, position, carries, rush_yds, nw_fpts_rush, nw_position)

nw_rec_wkly <- data %>% 
  dplyr::filter(!is.na(.data$receiver_player_id)) %>%
  dplyr::group_by(.data$receiver_player_id, .data$season, .data$week) %>%
  dplyr::summarize(
    player_name = dplyr::first(.data$receiver_player_name),
    recent_team = dplyr::first(.data$posteam),
    targets = dplyr::n(),
    rec_yds = sum(yards_gained),
    "rec_td_1" = sum(.data$td_player_id == .data$receiver_player_id & .data$yards_gained <= 19, na.rm = TRUE),
    "rec_td_20" = sum(.data$td_player_id == .data$receiver_player_id & .data$yards_gained >= 20 & yards_gained <= 49, na.rm = TRUE),
    "rec_td_50" = sum(.data$td_player_id == .data$receiver_player_id & .data$yards_gained >= 50, na.rm = TRUE),
    "rec_yds_100" = sum(ifelse(rec_yds >= 100 & rec_yds < 150, 1, 0)),
    "rec_yds_150" = sum(ifelse(rec_yds >= 150 & rec_yds < 200, 1, 0)),
    "rec_yds_200" = sum(ifelse(rec_yds >= 200, 1, 0)),
    "nw_fpts_rec" = sum(10 * rec_td_1,
                    15 * rec_td_20,
                    25 * rec_td_50,
                    10 * rec_yds_100,
                    15 * rec_yds_150,
                    25 * rec_yds_200)) %>% 
  rename(player_id = receiver_player_id) %>% 
  dplyr::filter(!is.na(.data$player_id)) %>% 
  ungroup() %>% 
  left_join(roster %>% select(gsis_id, season, position, full_name), by = c("player_id" = "gsis_id", "season")) %>% 
  mutate(player_name = if_else(is.na(full_name), full_name, player_name),
         nw_position = case_when(position == 'TE'| position == 'WR' ~ 'WR/TE', TRUE ~ position)) %>% 
  select(player_id:recent_team, position, targets, rec_yds, nw_fpts_rec, nw_position)

nw <- nw_rush_wkly %>% full_join(nw_rec_wkly %>% select(player_id:week, targets, rec_yds, nw_fpts_rec), by = c("player_id", "week", "season")) %>% group_by(player_id, season, week) %>% 
  mutate(touches = sum(carries, targets, na.rm = TRUE),
         combo_yds_100 = ifelse(rush_yds >= 50 & rec_yds >= 50, 10 , 0),
         combo_yds_150 = ifelse(rush_yds >= 75 & rec_yds >= 75, 10 , 0),
         nw_fpts_total = sum(nw_fpts_rush, nw_fpts_rec, combo_yds_150, combo_yds_100, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(-combo_yds_100, -combo_yds_150, -nw_position)

rb <- nw %>% 
  filter(position == "RB") %>% 
  group_by(player_id, season) %>% 
  dplyr::summarise(
    player_name = custom_mode(.data$player_name),
    recent_team = dplyr::last(.data$recent_team),
    position = last(position),
    games = dplyr::n(),
    nw_fpts_total = sum(nw_fpts_total),
    touches = sum(touches),
    carries = sum(carries),
    rush_yds = sum(rush_yds),
    nw_fpts_rush = sum(nw_fpts_rush),
    targets = sum(targets, na.rm = TRUE),
    rec_yds = sum(rec_yds, na.rm = TRUE),
    nw_fpts_rec = sum(nw_fpts_rec, na.rm = TRUE))
    
  
            

nw_rb_wkly <- nw_rush_wkly %>% 
  filter(position ==  "RB") %>% group_by(player_id, season) %>% summarise(player_name = last
                                                                           carries = sum(carries),
                                                                           rush_yds = sum(rush_yds),
                                                                           nw_fpts = sum(nw_fpts))


season_stats <- read_csv("season_stats.csv")

rb <- season_stats %>% filter(position == "RB") %>% select(player_id:position, car, rush_yds, nw_fpts)
