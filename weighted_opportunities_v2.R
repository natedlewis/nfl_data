# prepare data ----

# load pbp
future::plan("multisession")
pbp <- nflfastR::load_pbp(2010:2020, qs = TRUE) %>% 
  progressr::with_progress()

options(scipen = 9999)

# filter out dead plays and post season games
data <- pbp %>%
  dplyr::filter(
    season_type == "REG",
    !is.na(.data$down),
    .data$play_type %in% c("pass", "qb_kneel", "qb_spike", "run")
  ) %>%
  decode_player_ids()

# load weekly stats
wkly_stats <- calculate_player_stats(data, weekly = TRUE)

# load weekly rushing stats
rush_stats <- calculate_player_stats(data %>% filter(.data$play_type %in% c("run", "qb_kneel")), weekly = TRUE)

# load weekly receiving stats
rec_stats <- calculate_player_stats(data %>% filter(!is.na(.data$receiver_player_id)), weekly = TRUE)

# load roster
roster <- fast_scraper_roster(2010:2020)

# games played
games <- wkly_stats %>% group_by(player_id, season) %>% summarise(games = sum(n()))

# rushing ----

# carries/rushing tds
rush <- data %>%
  dplyr::filter(.data$play_type %in% c("run", "qb_kneel")) %>%
  dplyr::group_by(.data$rusher_player_id, .data$season) %>%
  dplyr::summarize(
    name_rush = dplyr::first(.data$rusher_player_name),
    team_rush = dplyr::first(.data$posteam),
    total_carries = dplyr::n(),
    rz_carries = sum(total_carries & .data$yardline_100 <= 20 & .data$yardline_100 >= 10),
    inside_ten_carries = sum(total_carries & .data$yardline_100 <= 9),
    outside_rz_carries = sum(total_carries & .data$yardline_100 > 20),
    neu_carries = sum(total_carries & .data$wp > .20 & .data$wp < .80 & .data$down <= 2 & .data$qtr <= 2 & .data$half_seconds_remaining > 120, na.rm = TRUE),
    gt_carries = sum(total_carries & .data$wp < .20 & .data$wp > .80 & .data$down <= 2 & .data$qtr <= 2 & .data$half_seconds_remaining > 120, na.rm = TRUE),
    "rush_td_1" = sum(.data$td_player_id == .data$rusher_player_id & .data$yards_gained <= 9, na.rm = TRUE),
    "rush_td_10" = sum(.data$td_player_id == .data$rusher_player_id & .data$yards_gained >= 10 & yards_gained <= 29, na.rm = TRUE),
    "rush_td_30" = sum(.data$td_player_id == .data$rusher_player_id & .data$yards_gained >= 30 & yards_gained <= 49, na.rm = TRUE),
    "rush_td_50" = sum(.data$td_player_id == .data$rusher_player_id & .data$yards_gained >= 50, na.rm = TRUE)) %>% 
  rename(player_id = rusher_player_id)

# rushing milestones
rush_ms <- rush_stats %>% 
  group_by(player_id, season) %>% 
  summarise(
    "rush_yds_100" = sum(.data$rushing_yards >= 100 & .data$rushing_yards <= 149),
    "rush_yds_150" = sum(.data$rushing_yards >= 150 & .data$rushing_yards <= 199),
    "rush_yds_200" = sum(.data$rushing_yards >= 200),
    conversions = sum(.data$rushing_2pt_conversions),
    std_fpts_rush = sum(fantasy_points)) %>% 
  dplyr::filter(!is.na(.data$player_id)) %>% 
  ungroup()

# combo yds/games played
combo_ms <- wkly_stats %>% 
  group_by(player_id, season) %>% 
  summarise(
    "combo_yds_100" = sum(.data$rushing_yards >= 50 & .data$receiving_yards >= 50),
    "combo_yds_150" = sum(.data$rushing_yards >= 75 & .data$receiving_yards >= 75)) %>% 
  dplyr::filter(!is.na(.data$player_id)) %>% 
  ungroup()

# join to single object
joined_rush <- rush %>% 
  left_join(rush_ms, by = c("player_id", "season")) %>% 
  left_join(combo_ms, by = c("player_id", "season"))

# detect na values
na_rush <- is.na(joined_rush)
joined_rush[na_rush] <- 0

# create nw_fpts from rushing plays
joined_rush <- joined_rush %>% 
  group_by(player_id, season) %>% 
  mutate(nw_fpts_combo = sum(10 * combo_yds_100, 10 * combo_yds_150),
         nw_fpts_rush = sum(10 * rush_td_1,
                            15 * rush_td_10,
                            20 * rush_td_30,
                            25 * rush_td_50,
                            10 * rush_yds_100,
                            15 * rush_yds_150,
                            25 * rush_yds_200,
                            nw_fpts_combo/2)) %>% 
  select(-nw_fpts_combo)

# receiving ----

# targets/receiving tds
rec <- data %>%
  dplyr::filter(!is.na(.data$receiver_player_id)) %>%
  dplyr::group_by(.data$receiver_player_id, .data$season) %>%
  dplyr::summarize(
    name_receiver = dplyr::first(.data$receiver_player_name),
    team_receiver = dplyr::first(.data$posteam),
    total_tgts = dplyr::n(),
    rz_tgts = sum(total_tgts & yardline_100 <= 20),
    outside_rz_tgts = total_tgts - rz_tgts,
    ez_tgts_inside_rz = sum(total_tgts & .data$yardline_100 == .data$air_yards & yardline_100 <= 20),
    ez_tgts_outside_rz = sum(total_tgts & .data$yardline_100 == .data$air_yards & yardline_100 >= 20),
    short_tgts = sum(total_tgts & .data$air_yards <= 5),
    deep_tgts = sum(total_tgts & .data$air_yards >= 20),
    "rec_td_1" = sum(.data$td_player_id == .data$receiver_player_id & yards_gained <= 19, na.rm = TRUE),
    "rec_td_20" = sum(.data$td_player_id == .data$receiver_player_id & yards_gained >= 20 & yards_gained <= 49, na.rm = TRUE),
    "rec_td_50" = sum(.data$td_player_id == .data$receiver_player_id & yards_gained >= 50, na.rm = TRUE)) %>% 
  rename(player_id = receiver_player_id)

# receiving milestones/std fpts
rec_ms <- rec_stats %>% 
  group_by(player_id, season) %>% 
  summarise(
    "rec_yds_100" = sum(.data$receiving_yards >= 100 & .data$receiving_yards <= 149),
    "rec_yds_150" = sum(.data$receiving_yards >= 150 & .data$receiving_yards <= 199),
    "rec_yds_200" = sum(.data$receiving_yards >= 200),
    conversions = sum(.data$receiving_2pt_conversions),
    std_fpts_rec = sum(fantasy_points)) %>% 
  dplyr::filter(!is.na(.data$player_id)) %>% 
  ungroup()

# join to single object
joined_rec <- rec %>% 
  left_join(rec_ms, by = c("player_id", "season")) %>% 
  left_join(combo_ms, by = c("player_id", "season"))

# detetc na values and remove
na_rec <- is.na(joined_rec)
joined_rec[na_rec] <- 0

# create nw_fpts from receiving plays
joined_rec <- joined_rec %>% 
  group_by(player_id, season) %>% 
  mutate(nw_fpts_combo = sum(10 * combo_yds_100, 10 * combo_yds_150),
         nw_fpts_rec = sum(10 * rec_td_1,
                            15 * rec_td_20,
                            25 * rec_td_50,
                            10 * rec_yds_100,
                            15 * rec_yds_150,
                            25 * rec_yds_200,
                            nw_fpts_combo/2)) %>% 
  select(-nw_fpts_combo)

# join ----

# join objects
joined_all <- joined_rush %>%
  dplyr::full_join(joined_rec, by = c("player_id", "season")) %>%
  dplyr::mutate(
    player_name = dplyr::case_when(
      !is.na(.data$name_rush) ~ .data$name_rush,
      !is.na(.data$name_receiver) ~ .data$name_receiver,
    ),
    recent_team = dplyr::case_when(
      !is.na(.data$team_rush) ~ .data$team_rush,
      !is.na(.data$team_receiver) ~ .data$team_receiver,
    )
  ) %>%
  dplyr::select(tidyselect::any_of(c(
    
    # id information
    "player_id", "player_name", "recent_team", "season",
    
    # rushing stats
    "total_carries", "rz_carries", "inside_ten_carries", 
    "outside_rz_carries", "neu_carries", "gt_carries", "nw_fpts_rush", "std_fpts_rush",
    
    # receiving stats
    "total_tgts", "rz_tgts", "outside_rz_tgts", "ez_tgts_inside_rz",
    "ez_tgts_outside_rz", "short_tgts", "deep_tgts", "nw_fpts_rec", "std_fpts_rec"
    
  ))) %>%
  dplyr::filter(!is.na(.data$player_id))

joined_all_nas <- is.na(joined_all)
joined_all[joined_all_nas] <- 0

# join roster
joined_all <- joined_all %>% 
  left_join(roster %>% select(gsis_id, season, position, full_name), by = c("player_id" = "gsis_id", "season")) %>% 
  left_join(games, by = c("player_id", "season"))

# filter to offense
offense <- joined_all %>% 
  filter(position %in% c("QB", "RB", "WR", "TE")) %>% 
  mutate(nw_position = case_when(position == 'TE'| position == 'WR' ~ 'WR/TE', TRUE ~ position),
         touches = sum(total_tgts, total_carries),
         player_name = ifelse(is.na(full_name), player_name, full_name)) %>% 
  select(season, player_name, recent_team, position, games, touches, total_carries:std_fpts_rec, player_id, nw_position) %>% 
  mutate(across(where(is.numeric), round, 2))

# running backs ----

# filter to running backs
rb <- offense %>% 
  filter(position == "RB", touches/games > 8) %>% 
  mutate(fpts_per_carry = nw_fpts_rush/total_carries,
         fpts_per_outside_rz_carry = nw_fpts_rush/outside_rz_carries,
         carry_opps = total_carries * fpts_per_carry,
         fpts_per_rz_carry = nw_fpts_rush/rz_carries,
         fpts_per_inside_ten_carry = nw_fpts_rush/inside_ten_carries,
         fpts_per_neu_carry = nw_fpts_rush/neu_carries,
         fpts_per_gt_carry = nw_fpts_rush/gt_carries,
         fpts_per_tgt = nw_fpts_rec/total_tgts,
         fpts_per_rz_tgt = nw_fpts_rec/rz_tgts,
         fpts_per_outside_rz_tgt = nw_fpts_rec/outside_rz_tgts,
         fpts_per_ez_tgt = nw_fpts_rec/(ez_tgts_inside_rz + ez_tgts_outside_rz),
         fpts_per_short_tgt = nw_fpts_rec/short_tgts,
         fpts_per_deep_tgt = nw_fpts_rec/deep_tgts
         ) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  arrange(-touches) %>% 
  ungroup()

is.na(rb) <- sapply(rb, is.infinite)
rb[is.na(rb)] <- NA

# average fpts per opportunity by type across past 10 seasons
season_avg <- rb %>% 
  group_by(position, season) %>% 
  summarise(avg_fpts_per_carry = mean(fpts_per_carry, trim = 0.2, na.rm = TRUE),
            avg_fpts_per_rz_carry = mean(fpts_per_rz_carry, trim = 0.2, na.rm = TRUE),
            avg_fpts_per_outside_rz_carry = mean(fpts_per_outside_rz_carry, trim = 0.2, na.rm = TRUE),
            avg_fpts_per_inside_ten_carry = mean(fpts_per_inside_ten_carry, trim = 0.2, na.rm = TRUE),
            avg_fpts_per_neu_carry = mean(fpts_per_neu_carry, trim = 0.2, na.rm = TRUE),
            avg_fpts_per_gt_carry = mean(fpts_per_gt_carry, trim = 0.2, na.rm = TRUE),
            avg_fpts_per_tgt = mean(fpts_per_tgt, trim = 0.2, na.rm = TRUE),
            avg_fpts_per_rz_tgt = mean(fpts_per_rz_tgt, trim = 0.2, na.rm = TRUE),
            avg_fpts_per_outside_rz_tgt = mean(fpts_per_outside_rz_tgt, trim = 0.2, na.rm = TRUE),
            avg_fpts_per_ez_tgt = mean(fpts_per_ez_tgt, trim = 0.2, na.rm = TRUE),
            avg_fpts_per_short_tgt = mean(fpts_per_short_tgt, trim = 0.2, na.rm = TRUE),
            avg_fpts_per_deep_tgt = mean(fpts_per_deep_tgt, trim = 0.2, na.rm = TRUE)) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  ungroup()

# aggregate average
recent_avg <- season_avg %>% 
  filter(season >= 2018) %>%
  group_by(position) %>% 
  summarise(avg_fpts_per_carry = mean(avg_fpts_per_carry),
            avg_fpts_per_rz_carry = mean(avg_fpts_per_rz_carry),
            avg_fpts_per_outside_rz_carry = mean(avg_fpts_per_outside_rz_carry),
            avg_fpts_per_inside_ten_carry = mean(avg_fpts_per_inside_ten_carry),
            avg_fpts_per_neu_carry = mean(avg_fpts_per_neu_carry),
            avg_fpts_per_gt_carry = mean(avg_fpts_per_gt_carry),
            avg_fpts_per_tgt = mean(avg_fpts_per_tgt),
            avg_fpts_per_rz_tgt = mean(avg_fpts_per_rz_tgt),
            avg_fpts_per_outside_rz_tgt = mean(avg_fpts_per_outside_rz_tgt),
            avg_fpts_per_ez_tgt = mean(avg_fpts_per_ez_tgt),
            avg_fpts_per_short_tgt = mean(avg_fpts_per_short_tgt),
            avg_fpts_per_deep_tgt = mean(avg_fpts_per_deep_tgt)) %>% 
  mutate(across(where(is.numeric), round, 2))

rb_opps <- rb %>% left_join(recent_avg, by = c("position")) %>% group_by(player_id, season) %>% 
  summarise(player_name = last(player_name),
            position = last(position),
            games = games,
            nw_fpts = sum(nw_fpts_rush),
            carry_opps = sum(total_carries * avg_fpts_per_carry),
            rz_opps = sum(rz_carries * avg_fpts_per_rz_carry),
            inside_ten_opps = sum(inside_ten_carries * avg_fpts_per_inside_ten_carry)) %>% 
  dplyr::arrange(-nw_fpts) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(position, season) %>%
  dplyr::mutate(rk = 1:n(),
                new_fpts = nw_fpts - carry_opps) %>% 
  select(rk, player_name:games, nw_fpts, carry_opps:inside_ten_opps, new_fpts) %>% 
  mutate(across(where(is.numeric), round, 0))
                         
                         

# bind rows to single object
avg <- season_avg %>% bind_rows(agg_avg) %>% bind_rows(recent_avg)

# pivot longer
avg_longer <- pivot_longer(avg, cols = 3:15) %>% filter(season == 3) %>% mutate(season = "Last 3")

# season averages rb opps
rb_per_game <- offense_df %>% 
  filter(position == "RB") %>% 
  group_by(player_id) %>% 
  summarise(player_name = last(player_name),
            position = last(position),
            seasons = n(),
            last_season = last(season),
            games = sum(games),
            "nw_fpts/g" = sum(nw_fpts_rush, nw_fpts_rec)/games,
            "nw_fpts_rush/g" = mean(nw_fpts_rush/games),
            "nw_fpts_rec/g" = mean(nw_fpts_rec/games),
            "carries/g" = mean(total_carries/games),
            "rz_carries/g" = mean(rz_carries/games),
            "outside_rz_carries/g" = mean(outside_rz_carries/games)) %>% 
  filter(games/(seasons*16) >= 0.2) %>% 
  mutate(across(where(is.numeric), round, 2))

# wide receivers/tight ends ----

wr_te <- offense_df %>% 
  select(season:games, total_tgts:nw_position) %>% 
  filter(nw_position == "WR/TE" & total_tgts/games > 2) %>% 
  mutate(fpts_per_tgt = nw_fpts_rec/total_tgts,
         fpts_per_rz_tgt = nw_fpts_rec/rz_tgts,
         fpts_per_outside_rz_tgt = nw_fpts_rec/outside_rz_tgts,
         fpts_per_ez_tgt_inside_rz = nw_fpts_rec/ez_tgts_inside_rz,
         fpts_per_ez_tgt_outside_rz = nw_fpts_rec/ez_tgts_outside_rz,
         fpts_per_short_tgt = nw_fpts_rec/short_tgts,
         fpts_per_deep_tgt = nw_fpts_rec/deep_tgts
  ) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  arrange(-total_tgts)

is.na(wr_te) <- sapply(wr_te, is.infinite)
wr_te[is.na(wr_te)] <- NA

# average fpts per opportunity by type across past 10 seasons
season_avg_rec <- wr_te %>% 
  group_by(position, season) %>% 
  summarise(avg_fpts_per_tgt = mean(fpts_per_tgt, trim = 0.05, na.rm = TRUE),
            avg_fpts_per_rz_tgt = mean(fpts_per_rz_tgt, trim = 0.05, na.rm = TRUE),
            avg_fpts_per_outside_rz_tgt = mean(fpts_per_outside_rz_tgt, trim = 0.05, na.rm = TRUE),
            avg_fpts_per_ez_tgt_inside_rz = mean(fpts_per_ez_tgt_inside_rz, trim = 0.05, na.rm = TRUE),
            avg_fpts_per_ez_tgt_outside_rz = mean(fpts_per_ez_tgt_outside_rz, trim = 0.05, na.rm = TRUE),
            avg_fpts_per_short_tgt = mean(fpts_per_short_tgt, trim = 0.05, na.rm = TRUE),
            avg_fpts_per_deep_tgt = mean(fpts_per_deep_tgt, trim = 0.05, na.rm = TRUE)) %>% 
  mutate(across(where(is.numeric), round, 2))

# aggregate average
agg_avg_rec <- season_avg_rec %>% 
  group_by(position) %>% 
  summarise(season = 10,
            avg_fpts_per_tgt = mean(avg_fpts_per_tgt),
            avg_fpts_per_rz_tgt = mean(avg_fpts_per_rz_tgt),
            avg_fpts_per_outside_rz_tgt = mean(avg_fpts_per_outside_rz_tgt),
            avg_fpts_per_ez_tgt_inside_rz = mean(avg_fpts_per_ez_tgt_inside_rz),
            avg_fpts_per_ez_tgt_outside_rz = mean(avg_fpts_per_ez_tgt_outside_rz),
            avg_fpts_per_short_tgt = mean(avg_fpts_per_short_tgt),
            avg_fpts_per_deep_tgt = mean(avg_fpts_per_deep_tgt)) %>% 
  mutate(across(where(is.numeric), round, 2))

recent_avg_rec <- season_avg_rec %>% 
  filter(season >= 2018) %>% 
  group_by(position) %>% 
  summarise(season = 3,
            avg_fpts_per_tgt = mean(avg_fpts_per_tgt),
            avg_fpts_per_rz_tgt = mean(avg_fpts_per_rz_tgt),
            avg_fpts_per_outside_rz_tgt = mean(avg_fpts_per_outside_rz_tgt),
            avg_fpts_per_ez_tgt_inside_rz = mean(avg_fpts_per_ez_tgt_inside_rz),
            avg_fpts_per_ez_tgt_outside_rz = mean(avg_fpts_per_ez_tgt_outside_rz),
            avg_fpts_per_short_tgt = mean(avg_fpts_per_short_tgt),
            avg_fpts_per_deep_tgt = mean(avg_fpts_per_deep_tgt)) %>% 
  mutate(across(where(is.numeric), round, 2))

# bind rows to single object
avg_rec <- season_avg_rec %>% bind_rows(agg_avg_rec) %>% bind_rows(recent_avg_rec)

# pivot longer
avg_longer_rec <- pivot_longer(avg_rec, cols = 3:9) %>% filter(season == 3) %>% mutate(season = "Last 3")

# bind ----

# bind rb and receiver frames
avg_longer_all <- avg_longer_rec %>% bind_rows(avg_longer) %>% arrange(position, -value)

# notes
# compare to std scoring
# create weighted opportunities variable
# create opportunity share variable
# how do coaches use their rz opporuntiies
# create qb frame