# create additional stats by play type -----------------------------------------------------------

# passing stats
pass <- data %>%
  dplyr::filter(.data$play_type %in% c("pass", "qb_spike")) %>%
  dplyr::group_by(.data$passer_player_id, .data$week, .data$season) %>%
  dplyr::summarize(
    name_pass = dplyr::first(.data$passer_player_name),
    team_pass = dplyr::first(.data$posteam),
    attempts = sum(.data$complete_pass == 1 | .data$incomplete_pass == 1 | .data$interception == 1),
    completions = sum(.data$complete_pass == 1),
    rz_att = sum(.data$attempts & yardline_100 <= 20, na.rm = TRUE),
    ez_att = sum(.data$attempts & .data$yardline_100 == .data$air_yards, na.rm = TRUE),
    deep_att = sum(.data$attempts & .data$air_yards >= 20, na.rm = TRUE),
    rz_cmp = sum(.data$complete_pass == 1 & yardline_100 <= 20, na.rm = TRUE),
    ez_cmp = sum(.data$complete_pass == 1 & .data$yardline_100 == .data$air_yards, na.rm = TRUE),
    deep_cmp = sum(.data$complete_pass == 1 & .data$air_yards >= 20, na.rm = TRUE),
    rz_cmp_rate = ifelse(rz_att >= 1, rz_cmp/rz_att, NA),
    ez_cmp_rate = ifelse(ez_att >= 1, ez_cmp/ez_att, NA),
    deep_cmp_rate = ifelse(deep_att >= 1, deep_cmp/deep_att, NA),
    "pass_td_1" = sum(.data$touchdown == 1 & .data$td_team == .data$posteam & .data$complete_pass == 1 & .data$yards_gained <= 19),
    "pass_td_20" = sum(.data$touchdown == 1 & .data$td_team == .data$posteam & .data$complete_pass == 1 & .data$yards_gained >= 20 & .data$yards_gained <= 49),
    "pass_td_50" = sum(.data$touchdown == 1 & .data$td_team == .data$posteam & .data$complete_pass == 1 & .data$yards_gained >= 50),
    int_tds = sum(.data$interception & .data$touchdown == 1 & .data$td_team == .data$defteam)
  ) %>%
  dplyr::rename(player_id = .data$passer_player_id) %>%
  dplyr::ungroup()

# rushing stats
rush <- data %>%
  dplyr::filter(.data$play_type %in% c("run", "qb_kneel")) %>%
  dplyr::group_by(.data$rusher_player_id, .data$week, .data$season) %>%
  dplyr::summarize(
    name_rush = dplyr::first(.data$rusher_player_name),
    team_rush = dplyr::first(.data$posteam),
    carries = dplyr::n(),
    big_runs = sum(n() & .data$yards_gained >= 15),
    in_five_carries = sum(carries & .data$yardline_100 <= 5),
    in_ten_carries = sum(carries & .data$yardline_100 <= 10),
    neu_carries = sum(carries & .data$wp > .20 & .data$wp < .80 & .data$down <= 2 & .data$qtr <= 2 & .data$half_seconds_remaining > 120, na.rm = TRUE),
    "rush_td_1" = sum(.data$td_player_id == .data$rusher_player_id & .data$yards_gained <= 9, na.rm = TRUE),
    "rush_td_10" = sum(.data$td_player_id == .data$rusher_player_id & .data$yards_gained >= 10 & yards_gained <= 29, na.rm = TRUE),
    "rush_td_30" = sum(.data$td_player_id == .data$rusher_player_id & .data$yards_gained >= 30 & yards_gained <= 49, na.rm = TRUE),
    "rush_td_50" = sum(.data$td_player_id == .data$rusher_player_id & .data$yards_gained >= 50, na.rm = TRUE)
  ) %>%
  dplyr::rename(player_id = .data$rusher_player_id) %>%
  dplyr::ungroup()

# receiving stats
rec <- data %>%
  dplyr::filter(!is.na(.data$receiver_player_id)) %>%
  dplyr::group_by(.data$receiver_player_id, .data$week, .data$season) %>%
  dplyr::summarize(
    name_receiver = dplyr::first(.data$receiver_player_name),
    team_receiver = dplyr::first(.data$posteam),
    targets = dplyr::n(),
    receptions = sum(.data$complete_pass == 1),
    rz_tgts = sum(.data$targets & yardline_100 <= 20),
    ez_tgts = sum(.data$targets & .data$yardline_100 == .data$air_yards),
    deep_tgts = sum(.data$targets & .data$air_yards >= 20),
    rz_rec = sum(.data$complete_pass == 1 & yardline_100 <= 20),
    ez_rec = sum(.data$complete_pass == 1 & .data$yardline_100 == .data$air_yards),
    deep_rec = sum(.data$complete_pass == 1 & .data$air_yards >= 20),
    rz_rec_rate = ifelse(rz_tgts >= 1, rz_rec/rz_tgts, NA),
    ez_rec_rate = ifelse(ez_tgts >= 1, ez_rec/ez_tgts, NA),
    deep_rec_rate = ifelse(deep_tgts >= 1, deep_rec/deep_tgts, NA),
    receiving_air_yards = sum(.data$air_yards, na.rm = TRUE),
    adot = ifelse(targets >= 1, receiving_air_yards/targets, NA),
    catch_rate = ifelse(targets >= 1, receptions/targets, NA),
    "rec_td_1" = sum(.data$td_player_id == .data$receiver_player_id & yards_gained <= 19, na.rm = TRUE),
    "rec_td_20" = sum(.data$td_player_id == .data$receiver_player_id & yards_gained >= 20 & yards_gained <= 49, na.rm = TRUE),
    "rec_td_50" = sum(.data$td_player_id == .data$receiver_player_id & yards_gained >= 50, na.rm = TRUE)
  ) %>%
  dplyr::rename(player_id = .data$receiver_player_id) %>%
  dplyr::ungroup()

# st tds
st <- pbp %>%
  dplyr::filter(.data$special == 1 & !is.na(.data$td_player_id)) %>%
  dplyr::group_by(.data$td_player_id, .data$week, .data$season) %>%
  dplyr::summarise(
    name_st = .data$td_player_name,
    team_st = .data$td_team,
    "st_td_1" = sum(.data$touchdown & return_yards <= 9, na.rm = TRUE),
    "st_td_10" = sum(.data$touchdown & return_yards >= 10 & return_yards <= 29, na.rm = TRUE),
    "st_td_30" = sum(.data$touchdown & return_yards >= 30 & return_yards <= 49, na.rm = TRUE),
    "st_td_50" = sum(.data$touchdown & return_yards >= 50, na.rm = TRUE)
  ) %>%
  dplyr::rename(player_id = .data$td_player_id)

# join all into single object -----------------------------------------------------------

joined_adv <- pass %>% 
  full_join(rush, by = c("player_id", "week", "season")) %>% 
  full_join(rec, by = c("player_id", "week", "season")) %>% 
  full_join(st, by = c("player_id", "week", "season")) %>%
  left_join(s_type, by = c("season", "week")) %>% 
  group_by(player_id, season, week) %>% 
  mutate(touches = sum(targets, carries, na.rm = TRUE)) %>% 
  ungroup() %>% 
  dplyr::mutate(
    player_name = dplyr::case_when(
      !is.na(.data$name_pass) ~ .data$name_pass,
      !is.na(.data$name_rush) ~ .data$name_rush,
      !is.na(.data$name_receiver) ~ .data$name_receiver,
    ),
    recent_team = dplyr::case_when(
      !is.na(.data$team_pass) ~ .data$team_pass,
      !is.na(.data$team_rush) ~ .data$team_rush,
      !is.na(.data$team_receiver) ~ .data$team_receiver,
    )
  ) %>% 
dplyr::select(tidyselect::any_of(c(
  
  # id information
  "player_id", "player_name", "recent_team", "season", "week",
  
  # passing stats
  "rz_att", "ez_att", "deep_att", "rz_cmp", "ez_cmp", "deep_cmp", 
  "rz_cmp_rate", "ez_cmp_rate", "deep_cmp_rate", "pass_td_1", "pass_td_20", "pass_td_50", "int_tds",
  
  # rushing stats
  "touches", "big_runs", "in_five_carries", "in_ten_carries", "neu_carries", "rush_td_1", "rush_td_10", 
  "rush_td_30", "rush_td_50",
  
  # receiving stats
  "rz_tgts", "ez_tgts", "deep_tgts", "rz_rec", "ez_rec",
  "deep_rec",  "catch_rate", "adot", "rz_rec_rate", "ez_rec_rate", "deep_rec_rate",
  "rec_td_1", "rec_td_20", "rec_td_50",
  
  # st stats
  "st_td_1", "st_td_10", "st_td_30", "st_td_50"

))) %>%
  dplyr::filter(!is.na(.data$player_id))

# advanced weekly -----------------------------------------------------------
adv_wkly <- joined_adv %>% 
  dplyr::select(tidyselect::any_of(c(
    
    # id information
    "player_id", "player_name", "recent_team", "season", "week",
    
    # passing stats
    "rz_att", "ez_att", "deep_att", "rz_cmp", "ez_cmp", "deep_cmp", 
    "rz_cmp_rate", "ez_cmp_rate", "deep_cmp_rate",
    
    # rushing stats
    "touches", "big_runs", "in_five_carries", "in_ten_carries", "neu_carries",
    
    # receiving stats
    "rz_tgts", "ez_tgts", "deep_tgts", "rz_rec", "ez_rec",
    "deep_rec",  "catch_rate", "adot", "rz_rec_rate", "ez_rec_rate", "deep_rec_rate"
  )))

# create yardage milestones -----------------------------------------------------------

ms <- raw_wkly %>% 
  group_by(player_id, season, week) %>% 
  summarise(
    
    # yardage milestones
    "pass_yds_300" = sum(.data$passing_yards >= 300 & .data$passing_yards <= 349),
    "pass_yds_350" = sum(.data$passing_yards >= 350 & .data$passing_yards <= 399),
    "pass_yds_400" = sum(.data$passing_yards >= 400),
    "rush_yds_100" = sum(.data$rushing_yards >= 100 & .data$rushing_yards <= 149),
    "rush_yds_150" = sum(.data$rushing_yards >= 150 & .data$rushing_yards <= 199),
    "rush_yds_200" = sum(.data$rushing_yards >= 200),
    "rec_yds_100" = sum(.data$receiving_yards >= 100 & .data$receiving_yards <= 149),
    "rec_yds_150" = sum(.data$receiving_yards >= 150 & .data$receiving_yards <= 199),
    "rec_yds_200" = sum(.data$receiving_yards >= 200),
    "combo_yds_100" = sum(.data$rushing_yards >= 50 & .data$receiving_yards >= 50),
    "combo_yds_150" = sum(.data$rushing_yards >= 75 & .data$receiving_yards >= 75),
    
    # other
    conversions = sum(.data$passing_2pt_conversions + .data$rushing_2pt_conversions + .data$receiving_2pt_conversions),
    interceptions = sum(interceptions)) %>% 
  dplyr::filter(!is.na(.data$player_id)) %>% 
  ungroup()

# nw weekly -----------------------------------------------------------

nw_wkly <- joined_adv %>% 
  full_join(ms, by = c("player_id", "season", "week")) %>% 
  
# select only nw fantasy relevant variables
  dplyr::select(tidyselect::any_of(c(
    
    # id information
    "player_id", "player_name", "recent_team", "season", "week",
    
    # passing stats
    "pass_yds_300", "pass_yds_350", "pass_yds_400",
    "pass_td_1", "pass_td_20", "pass_td_50", "int_tds",
    
    # rushing stats
    "rush_yds_100", "rush_yds_150", "rush_yds_200",
    "rush_td_1", "rush_td_10", "rush_td_30", "rush_td_50",
    
    # receiving stats
    "rec_yds_100", "rec_yds_150", "rec_yds_200",
    "rec_td_1", "rec_td_20", "rec_td_50",
    
    # other stats
    "combo_yds_100", "combo_yds_150", "st_td_1", "st_td_10", "st_td_30", "st_td_50",
    "interceptions", "conversions"
  ))) %>% 
  
# create nwfpts variable
  mutate_all(~replace(., is.na(.), 0)) %>% 
  group_by(player_id, season, week) %>% 
  dplyr::mutate(
    nw_fantasy_points = 
      10 * pass_td_1 +
      15 * pass_td_20 +
      20 * pass_td_50 +
      10 * pass_yds_300 +
      15 * pass_yds_350 +
      25 * pass_yds_400 +
      -5 * interceptions +
      -15 * int_tds +
      3 * conversions +
      10 * rush_td_1 +
      15 * rush_td_10 +
      20 * rush_td_30 +
      25 * rush_td_50 +
      10 * rush_yds_100 + 
      15 * rush_yds_150 +
      25 * rush_yds_200 +
      10 * rec_td_1 +
      15 * rec_td_20 +
      20 * rec_td_50 +
      10 * rec_yds_100 +
      15 * rec_yds_150 +
      25 * rec_yds_200 +
      10 * combo_yds_100 +
      10 * combo_yds_150 +
      10 * st_td_1 +
      15 * st_td_10 +
      20 * st_td_30 +
      25 * st_td_50) %>% 
  select(-conversions, -interceptions) %>% 
  relocate(nw_fantasy_points, .before = pass_yds_300)

# next gen stats ---------------------------------------------------------------

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

# remove stale objects -----------------------------------------------------------

rm(pass, rush, rec, st, joined_adv, ms, ngs)
