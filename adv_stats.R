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
    pass_yds = sum(yards_gained),
    rz_att = sum(.data$attempts & yardline_100 <= 20, na.rm = TRUE),
    ez_att = sum(.data$attempts & .data$yardline_100 == .data$air_yards, na.rm = TRUE),
    ez_att_inside_rz = sum(.data$attempts & .data$yardline_100 == .data$air_yards, yardline_100 <= 20, na.rm = TRUE),
    ez_att_outside_rz = sum(.data$attempts & .data$yardline_100 == .data$air_yards, yardline_100 > 20, na.rm = TRUE),
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
    "pass_yds_300" = sum(ifelse(pass_yds >= 300 & pass_yds < 350, 1, 0)),
    "pass_yds_350" = sum(ifelse(pass_yds >= 350 & pass_yds < 400, 1, 0)),
    "pass_yds_400" = sum(ifelse(pass_yds >= 400, 1, 0)),
    int_tds = sum(.data$interception & .data$touchdown == 1 & .data$td_team == .data$defteam)
  ) %>%
  dplyr::rename(player_id = .data$passer_player_id) %>%
  dplyr::ungroup()

pass_nas <- is.na(pass)
pass[pass_nas] <- 0

# rushing stats
rush <- data %>%
  dplyr::filter(.data$play_type %in% c("run", "qb_kneel")) %>%
  dplyr::group_by(.data$rusher_player_id, .data$week, .data$season) %>%
  dplyr::summarize(
    name_rush = dplyr::first(.data$rusher_player_name),
    team_rush = dplyr::first(.data$posteam),
    carries = dplyr::n(),
    rush_yds = sum(yards_gained),
    rz_carries = sum(carries & .data$yardline_100 <= 20),
    outside_rz_carries = sum(carries & .data$yardline_100 > 20),
    inside_five_carries = sum(carries & .data$yardline_100 < 5),
    inside_ten_carries = sum(carries & .data$yardline_100 < 10),
    neu_carries = sum(carries & .data$wp > .20 & .data$wp < .80 & .data$down <= 2 & .data$qtr <= 2 & .data$half_seconds_remaining > 120, na.rm = TRUE),
    "rush_td_1" = sum(.data$td_player_id == .data$rusher_player_id & .data$yards_gained <= 9, na.rm = TRUE),
    "rush_td_10" = sum(.data$td_player_id == .data$rusher_player_id & .data$yards_gained >= 10 & yards_gained <= 29, na.rm = TRUE),
    "rush_td_30" = sum(.data$td_player_id == .data$rusher_player_id & .data$yards_gained >= 30 & yards_gained <= 49, na.rm = TRUE),
    "rush_td_50" = sum(.data$td_player_id == .data$rusher_player_id & .data$yards_gained >= 50, na.rm = TRUE),
    "rush_yds_100" = sum(ifelse(rush_yds >= 100 & rush_yds < 150, 1, 0)),
    "rush_yds_150" = sum(ifelse(rush_yds >= 150 & rush_yds < 200, 1, 0)),
    "rush_yds_200" = sum(ifelse(rush_yds >= 200, 1, 0))
  ) %>%
  dplyr::rename(player_id = .data$rusher_player_id) %>%
  dplyr::ungroup()

rush_nas <- is.na(rush)
rush[rush_nas] <- 0

# receiving stats
rec <- data %>%
  dplyr::filter(!is.na(.data$receiver_player_id)) %>%
  dplyr::group_by(.data$receiver_player_id, .data$week, .data$season) %>%
  dplyr::summarize(
    name_receiver = dplyr::first(.data$receiver_player_name),
    team_receiver = dplyr::first(.data$posteam),
    targets = dplyr::n(),
    receptions = sum(.data$complete_pass == 1),
    rec_yds = sum(yards_gained),
    rz_tgts = sum(.data$targets & yardline_100 <= 20),
    outside_rz_tgts = sum(.data$targets & yardline_100 > 20),
    ez_tgts = sum(.data$targets & .data$yardline_100 == .data$air_yards),
    ez_tgts_inside_rz = sum(.data$targets & .data$yardline_100 == .data$air_yards & yardline_100 <= 20),
    ez_tgts_outside_rz = sum(.data$targets & .data$yardline_100 == .data$air_yards & yardline_100 > 20),
    deep_tgts = sum(.data$targets & .data$air_yards >= 20),
    receiving_air_yards = sum(.data$air_yards, na.rm = TRUE),
    adot = ifelse(targets >= 1, receiving_air_yards/targets, NA),
    catch_rate = ifelse(targets >= 1, receptions/targets, NA),
    "rec_td_1" = sum(.data$td_player_id == .data$receiver_player_id & yards_gained <= 19, na.rm = TRUE),
    "rec_td_20" = sum(.data$td_player_id == .data$receiver_player_id & yards_gained >= 20 & yards_gained <= 49, na.rm = TRUE),
    "rec_td_50" = sum(.data$td_player_id == .data$receiver_player_id & yards_gained >= 50, na.rm = TRUE),
    "rec_yds_100" = sum(ifelse(rec_yds >= 100 & rec_yds < 150, 1, 0)),
    "rec_yds_150" = sum(ifelse(rec_yds >= 150 & rec_yds < 200, 1, 0)),
    "rec_yds_200" = sum(ifelse(rec_yds >= 200, 1, 0))
  ) %>%
  dplyr::rename(player_id = .data$receiver_player_id) %>%
  dplyr::ungroup()

rec_nas <- is.na(rec)
rec[rec_nas] <- 0

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

joined <- pass %>% 
  full_join(rush, by = c("player_id", "week", "season")) %>% 
  full_join(rec, by = c("player_id", "week", "season")) %>% 
  full_join(st, by = c("player_id", "week", "season")) %>%
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
  "rz_att", "ez_att", "ez_att_inside_rz", "ez_att_outside_rz", "deep_att",
  "rz_cmp_rate", "ez_cmp_rate", "deep_cmp_rate", 
  "pass_td_1", "pass_td_20", "pass_td_50", "int_tds",
  "pass_yds_300", "pass_yds_350", "pass_yds_400",
  
  # rushing stats
  "touches", "outside_rz_carries", "rz_carries", "inside_ten_carries", "inside_five_carries", "neu_carries", 
  "rush_td_1", "rush_td_10", "rush_td_30", "rush_td_50", "rush_yds_100", "rush_yds_150", "rush_yds_200",
  
  # receiving stats
  "rz_tgts", "ez_tgts", "ez_targets_inside_rz",
  "ez_targets_outside_rz", "deep_tgts", "catch_rate", 
  "adot", "rz_rec_rate", "ez_rec_rate", "deep_rec_rate",
  "rec_yds_100", "rec_yds_150", "rec_yds_200",
  "rec_td_1", "rec_td_20", "rec_td_50",
  
  # st stats
  "st_td_1", "st_td_10", "st_td_30", "st_td_50"))) %>% 
  dplyr::filter(!is.na(.data$player_id),
                !is.na(player_name))

joined_nas <- is.na(joined)
joined[joined_nas] <- 0

# join stats and roster -----------------------------------------------------------

wkly <- raw_wkly %>% filter(week <= 17) %>% 
  left_join(roster, by = c("player_id" = "gsis_id", "season")) %>% 
  left_join(joined %>% select(-player_name, -recent_team))

wkly <- wkly %>% group_by(player_id, season, week) %>% 
  dplyr::mutate(
    player_name = ifelse(!is.na(full_name), full_name, player_name),
    combo_yds_100 = ifelse(rushing_yards >= 50 & receiving_yards >= 50, 1, 0),
    combo_yds_150 = ifelse(rushing_yards >= 75 & receiving_yards >= 75, 1, 0),
    conversions = rushing_2pt_conversions + rushing_2pt_conversions + passing_2pt_conversions,
    pass_pts =
      10 * pass_td_1 +
      15 * pass_td_20 +
      20 * pass_td_50 +
      10 * pass_yds_300 +
      15 * pass_yds_350 +
      25 * pass_yds_400 +
      -5 * interceptions +
      -15 * int_tds +
      3 * passing_2pt_conversions,
    combo_pts =
      10 * combo_yds_100 +
      10 * combo_yds_150,
    rush_pts =
      10 * rush_td_1 +
      15 * rush_td_10 +
      20 * rush_td_30 +
      25 * rush_td_50 +
      10 * rush_yds_100 + 
      15 * rush_yds_150 +
      25 * rush_yds_200 +
      3 * rushing_2pt_conversions,
    rec_pts =
      10 * rec_td_1 +
      15 * rec_td_20 +
      20 * rec_td_50 +
      10 * rec_yds_100 +
      15 * rec_yds_150 +
      25 * rec_yds_200 +
      3 * receiving_2pt_conversions,
    st_pts =
      10 * st_td_1 +
      15 * st_td_10 +
      20 * st_td_30 +
      25 * st_td_50,
    nw_pts = pass_pts + rush_pts + rec_pts + st_pts + combo_pts
    ) %>% 
  mutate(rush_pts = rush_pts + (combo_pts/2),
         rec_pts = rec_pts + (combo_pts/2)) %>% 
  ungroup() %>% 
  dplyr::select(tidyselect::any_of(c(
    
    # id information
    "player_id", "player_name", "recent_team", "position", "season", "week", "nw_pts",
    
    # passing stats
    "completions", "attempts", "passing_yards", "passing_tds", "interceptions",
    "sacks", "passing_air_yards", "passing_yards_after_catch",
    "passing_first_downs", "passing_epa", "dakota", "rz_att", "ez_att", "ez_att_inside_rz", 
    "ez_att_outside_rz", "deep_att", "rz_cmp_rate", "ez_cmp_rate", "deep_cmp_rate", 
    "pass_td_1", "pass_td_20", "pass_td_50", "int_tds",
    "pass_yds_300", "pass_yds_350", "pass_yds_400", "pass_pts",
    
    # rushing stats
    "carries", "rushing_yards", "rushing_tds", "rushing_first_downs", "rushing_epa",
    "touches", "outside_rz_carries", "rz_carries", "inside_ten_carries", 
    "inside_five_carries", "neu_carries", "rush_td_1", "rush_td_10", "rush_td_30", 
    "rush_td_50", "rush_yds_100", "rush_yds_150", "rush_yds_200", "rush_pts",
    
    # receiving stats
    "receptions", "targets", "receiving_yards", "receiving_tds",
    "receiving_air_yards", "receiving_yards_after_catch",
    "receiving_first_downs", "receiving_epa", "rz_tgts", "ez_tgts", "ez_tgts_inside_rz", "ez_tgts_outside_rz",
    "deep_tgts", "catch_rate", "adot", "rz_rec_rate", "ez_rec_rate", 
    "deep_rec_rate", "rec_yds_100", "rec_yds_150", "rec_yds_200",
    "combo_yds_100", "combo_yds_150",
    "rec_td_1", "rec_td_20", "rec_td_50", "rec_pts",
    
    # special teams
    "special_teams_tds", "fantasy_points"
  )))

# season stats ---------------------------------------------------------------

season <- wkly %>%
  dplyr::group_by(.data$player_id, season) %>%
  dplyr::summarise(
    player_name = custom_mode(.data$player_name),
    games = dplyr::n(),
    recent_team = dplyr::last(.data$recent_team),
    position = dplyr::last(.data$position),
    # passing
    completions = sum(.data$completions),
    attempts = sum(.data$attempts),
    passing_yards = sum(.data$passing_yards),
    passing_tds = sum(.data$passing_tds),
    interceptions = sum(.data$interceptions),
    sacks = sum(.data$sacks),
    passing_air_yards = sum(.data$passing_air_yards),
    passing_yards_after_catch = sum(.data$passing_yards_after_catch),
    passing_first_downs = sum(.data$passing_first_downs),
    passing_epa = dplyr::if_else(all(is.na(.data$passing_epa)), NA_real_, sum(.data$passing_epa, na.rm = TRUE)),
    rz_attempts = sum(rz_att),
    ez_attempts = sum(ez_att),
    ez_attempts_inside_rz = sum(ez_att_inside_rz),
    ez_attempts_outside_rz = sum(ez_att_outside_rz),
    deep_attempts = sum(deep_att),
    pass_yds_300 = sum(pass_yds_300),
    pass_yds_350 = sum(pass_yds_350),
    pass_yds_400 = sum(pass_yds_400),
    pass_td_1 = sum(pass_td_1),
    pass_td_20 = sum(pass_td_20),
    pass_td_50 = sum(pass_td_50),
    int_tds = sum(int_tds),
    
    # rushing
    carries = sum(.data$carries),
    rushing_yards = sum(.data$rushing_yards),
    rushing_tds = sum(.data$rushing_tds),
    rushing_first_downs = sum(.data$rushing_first_downs),
    rushing_epa = dplyr::if_else(all(is.na(.data$rushing_epa)), NA_real_, sum(.data$rushing_epa, na.rm = TRUE)),
    touches = sum(touches),
    rz_carries = sum(rz_carries),
    outside_rz_carries = sum(outside_rz_carries),
    inside_ten = sum(inside_ten_carries),
    inside_five = sum(inside_five_carries),
    neutral = sum(neu_carries),
    rush_yds_100 = sum(rush_yds_100),
    rush_yds_150 = sum(rush_yds_150),
    rush_yds_200 = sum(rush_yds_200),
    rush_td_1 = sum(rush_td_1),
    rush_td_10 = sum(rush_td_10),
    rush_td_30 = sum(rush_td_30),
    rush_td_50 = sum(rush_td_50),
    combo_yds_100 = sum(combo_yds_100),
    combo_yds_150 = sum(combo_yds_150),

    # receiving
    receptions = sum(.data$receptions),
    targets = sum(.data$targets),
    receiving_yards = sum(.data$receiving_yards),
    receiving_tds = sum(.data$receiving_tds),
    receiving_air_yards = sum(.data$receiving_air_yards),
    receiving_yards_after_catch = sum(.data$receiving_yards_after_catch),
    receiving_first_downs = sum(.data$receiving_first_downs),
    receiving_epa = dplyr::if_else(all(is.na(.data$receiving_epa)), NA_real_, sum(.data$receiving_epa, na.rm = TRUE)),
    rz_targets = sum(rz_tgts),
    outside_rz_targets = sum(targets - rz_targets),
    ez_targets = sum(ez_tgts),
    deep_targets = sum(deep_tgts),
    adot = mean(adot),
    catch_rate = mean(catch_rate),
    rec_yds_100 = sum(rec_yds_100),
    rec_yds_150 = sum(rec_yds_150),
    rec_yds_200 = sum(rec_yds_200),
    rec_td_1 = sum(rec_td_1),
    rec_td_20 = sum(rec_td_20),
    rec_td_50 = sum(rec_td_50),

    # special teams
    special_teams_tds = sum(.data$special_teams_tds),
    
    # fantasy
    std_pts = sum(.data$fantasy_points),
    nw_pts = sum(nw_pts),
    rush_pts = sum(rush_pts),
    rec_pts = sum(rec_pts),
  ) %>% 
  arrange(-nw_pts) %>% 
  dplyr::group_by(position, season) %>%
  dplyr::mutate(nw_rk = 1:n()) %>%
  arrange(-std_pts) %>% 
  dplyr::mutate(std_rk = 1:n()) %>%
  ungroup() %>% 
  mutate(across(where(is.numeric), round, 2))

# join adp data ----

# load adp
adp <- read_csv("adp_all.csv")

# select variables from adp df
adp <- adp %>% 
  rename(player_id = gsis_id) %>% 
  select(player_id, season, pick, overall) 

# join to season stats
season <- season %>% left_join(adp, by = c("player_id", "season"))

# weighted opportunites ----

rb <- season %>% filter(position == "RB", touches/games > 8) %>% 
  select(nw_rk, player_id:position, nw_pts, rush_pts, rec_pts, carries, touches:neutral, targets, rz_targets:deep_targets) %>% 
  mutate(per_carry = rush_pts/carries,
         per_outside_rz_carry = rush_pts/outside_rz_carries,
         per_rz_carry = ifelse(rz_carries > 0, rush_pts/rz_carries, NA),
         per_inside_ten = ifelse(inside_ten > 0, rush_pts/inside_ten, NA),
         per_inside_five = ifelse(inside_five > 0, rush_pts/inside_five, NA),
         per_touch = rush_pts/touches,
         per_neutral = ifelse(neutral > 0, rush_pts/neutral, NA),
         per_target = ifelse(targets > 0, rec_pts/targets, NA),
         per_outside_rz_target = ifelse(outside_rz_targets > 0, rec_pts/outside_rz_targets, NA),
         per_rz_target = ifelse(rz_targets > 0, rec_pts/rz_targets, NA),
         per_ez_target = ifelse(ez_targets > 0, rec_pts/ez_targets, NA),
         per_deep_target = ifelse(deep_targets > 0, rec_pts/deep_targets, NA),
         ) %>% 
  mutate(across(where(is.numeric), round, 2))

rb_nas <- is.na(rb)
rb[rb_nas] <- 0

rb_avg <- rb %>% 
  group_by(position) %>% 
  summarise(
    avg_per_carry = mean(per_carry, trim = 0.2, na.rm = TRUE),
    avg_per_outside_rz_carry = mean(per_outside_rz_carry, trim = 0.2, na.rm = TRUE),
    avg_per_rz_carry = mean(per_rz_carry, trim = 0.2, na.rm = TRUE),
    avg_per_inside_ten = mean(per_inside_ten, trim = 0.2, na.rm = TRUE),
    avg_per_inside_five = mean(per_inside_five, trim = 0.2, na.rm = TRUE),
    avg_per_touch = mean(per_touch, trim = 0.2, na.rm = TRUE),
    avg_per_neutral = mean(per_neutral, trim = 0.2, na.rm = TRUE),
    avg_per_target = mean(per_target, trim = 0.2, na.rm = TRUE),
    avg_per_outside_rz_target = mean(per_outside_rz_target, trim = 0.2, na.rm = TRUE),
    avg_per_rz_target = mean(per_rz_target, trim = 0.2, na.rm = TRUE),
    avg_per_ez_target = mean(per_ez_target, trim = 0.2, na.rm = TRUE),
    avg_per_deep_target = mean(per_deep_target, trim = 0.2, na.rm = TRUE))

opps <- rb %>% left_join(rb_avg, by = c("position")) %>% group_by(player_id, season) %>% 
  summarise(opps = outside_rz_carries * avg_per_outside_rz_carry +
            rz_carries - inside_ten * avg_per_rz_carry +
            inside_ten * avg_per_inside_ten +
            outside_rz_targets * avg_per_outside_rz_target +
            rz_targets * avg_per_rz_target +
            ez_targets * avg_per_ez_target
  )

rb <- rb %>% left_join(opps, by = c("player_id", "season"))