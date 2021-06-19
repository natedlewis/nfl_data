# prepare data ----

# load pbp
future::plan("multisession")
pbp <- nflfastR::load_pbp(2010:2020, qs = TRUE) %>% 
  progressr::with_progress()

options(scipen = 9999)

# filter out dead plays and post season games
data <- pbp %>%
  dplyr::filter(
    !is.na(.data$down),
    .data$play_type %in% c("pass", "qb_kneel", "qb_spike", "run")
  ) %>%
  decode_player_ids()

# load play data from various portions of the field
outside_rz_data <- data %>% filter(.data$yardline_100 > 20)
rz_data <- data %>% filter(.data$yardline_100 <= 20)
inside_10_data <- data %>% filter(.data$yardline_100 < 9)

# load stats
all_stats <- calculate_player_stats(data, weekly = TRUE)
outside_rz_stats <- calculate_player_stats(outside_rz_data, weekly = TRUE)
rz_stats <- calculate_player_stats(rz_data, weekly = TRUE)
inside_10_stats <- calculate_player_stats(inside_10_data, weekly = TRUE)

# load roster
roster <- fast_scraper_roster(2010:2020)

calculate_nw_rush <- function(outside_rz_data, data, rz_data, inside_10_data) {
input <- outside_rz_data
  
nw_rush <- outside_rz_data %>% 
  filter(.data$play_type %in% c("qb_kneel", "run")) %>% 
  dplyr::group_by(.data$rusher_player_id, .data$season, .data$week) %>%
  dplyr::summarize(
    player_name = dplyr::first(.data$rusher_player_name),
    recent_team = dplyr::first(.data$posteam),
    carries = dplyr::n(),
    "rush_td_1" = sum(.data$td_player_id == .data$rusher_player_id & .data$yards_gained <= 9, na.rm = TRUE),
    "rush_td_10" = sum(.data$td_player_id == .data$rusher_player_id & .data$yards_gained >= 10 & yards_gained <= 29, na.rm = TRUE),
    "rush_td_30" = sum(.data$td_player_id == .data$rusher_player_id & .data$yards_gained >= 30 & yards_gained <= 49, na.rm = TRUE),
    "rush_td_50" = sum(.data$td_player_id == .data$rusher_player_id & .data$yards_gained >= 50, na.rm = TRUE),
    "rush_yds" = sum(yards_gained),
    "rush_yds_100" = sum(ifelse(rush_yds >= 100 & rush_yds < 150, 1, 0)),
    "rush_yds_150" = sum(ifelse(rush_yds >= 150 & rush_yds < 200, 1, 0)),
    "rush_yds_200" = sum(ifelse(rush_yds >= 200, 1, 0)),
    "nw_fpts" = sum(10 * rush_td_1,
                    15 * rush_td_10,
                    20 * rush_td_30,
                    25 * rush_td_50,
                    10 * rush_yds_100,
                    15 * rush_yds_150,
                    25 * rush_yds_200)) %>% 
  rename(player_id = rusher_player_id) %>% 
  dplyr::filter(!is.na(.data$player_id)) %>% 
  ungroup()
  
nw_rush_nas <- is.na(nw_rush)
nw_rush[nw_rush_nas] <- 0

nw_rush <- nw_rush %>% 
  left_join(roster %>% select(gsis_id, season, position, full_name), by = c("player_id" = "gsis_id", "season")) %>% 
  mutate(player_name = ifelse(is.na(full_name), full_name, player_name),
         nw_position = case_when(position == 'TE'| position == 'WR' ~ 'WR/TE', TRUE ~ position)) %>% 
  select(player_id, season, week, player_name, recent_team, position, carries, rush_yds, nw_fpts, nw_position)
return(nw_rush)
}

nw_outside <- calculate_nw_rush(outside_rz_data) %>% 
  select(player_id:week, carries:nw_fpts) %>% 
  rename(outside_carries = carries, outside_rush_yds = rush_yds, outside_nw_fpts = nw_fpts)

nw_rz <- calculate_nw_rush(rz_data) %>% 
  select(player_id:week, carries:nw_fpts) %>% 
  rename(rz_carries = carries, rz_rush_yds = rush_yds, rz_nw_fpts = nw_fpts)

nw_inside_10 <- calculate_nw_rush(inside_10_data) %>% 
  select(player_id:week, carries:nw_fpts) %>% 
  rename(inside_10_carries = carries, inside_10_rush_yds = rush_yds, inside_10_nw_fpts = nw_fpts)

nw_all <- calculate_nw_rush(data) %>% 
  select(player_id:week, carries:nw_fpts) %>% 
  rename(total_carries = carries, total_rush_yds = rush_yds, total_nw_fpts = nw_fpts)

nw <- nw_all %>% left_join(nw_outside, by = c("player_id", "season", "week")) %>% 
  left_join(nw_rz, by = c("player_id", "season", "week")) %>% 
  left_join(nw_inside_10, by = c("player_id", "season", "week"))

fpts_per_outside_carry <- nw_outside %>% group_by(player_id, season) %>% 
  summarise(
    player_name = last(player_name),
    last_season = last(season),
    position = last(position),
    games = n(),
    carries = sum(carries),
    nw_fpts = sum(nw_fpts)) %>% 
  filter(position == "RB" & carries/games >= 5, season >= 2018) %>% 
  mutate(fpts_per_outside_carry = nw_fpts/carries) %>% 
  ungroup()

fpts_per_rz_carry <- nw_rz %>% group_by(player_id, season) %>% 
  summarise(
    player_name = last(player_name),
    last_season = last(season),
    position = last(position),
    games = n(),
    carries = sum(carries),
    nw_fpts = sum(nw_fpts),
    fpts_per_rz_carry = nw_fpts/carries) %>% 
  filter(position == "RB" & carries/games >= 1, season >= 2018)

fpts_per_inside_carry <- nw_inside_10 %>% group_by(player_id, season) %>% 
  summarise(
    player_name = last(player_name),
    last_season = last(season),
    position = last(position),
    games = n(),
    carries = sum(carries),
    nw_fpts = sum(nw_fpts),
    fpts_per_inside_carry = nw_fpts/carries) %>% 
  filter(position == "RB" & carries/games >= 0.5, season >= 2018)

avg_per_outside_carry <- mean(fpts_per_outside_carry$fpts_per_outside_carry, trim = 0.2, na.rm = TRUE)
avg_per_rz_carry <- mean(fpts_per_rz_carry$fpts_per_rz_carry, trim = 0.2, na.rm = TRUE)
avg_per_inside_carry <- mean(fpts_per_inside_carry$fpts_per_inside_carry, trim = 0.2, na.rm = TRUE)



carries <- data %>%
  dplyr::filter(.data$play_type %in% c("run", "qb_kneel")) %>%
  dplyr::group_by(.data$rusher_player_id, .data$season) %>%
  dplyr::summarize(
    player_name = dplyr::first(.data$rusher_player_name),
    recent_team = dplyr::first(.data$posteam),
    total_carries = dplyr::n(),
    rz_carries = sum(total_carries & .data$yardline_100 <= 20 & .data$yardline_100 >= 10),
    inside_ten_carries = sum(total_carries & .data$yardline_100 <= 9),
    outside_rz_carries = sum(total_carries & .data$yardline_100 > 20)) %>% 
  rename(player_id = rusher_player_id)

# combo yds/games played
combo_yds <- outside_rz_stats %>% 
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
outside_rz_fpts <- joined_rush %>% 
  group_by(player_id, season) %>% 
  summarise(nw_fpts_combo = sum(10 * combo_yds_100, 10 * combo_yds_150),
         nw_fpts_rush = sum(10 * rush_td_1,
                            15 * rush_td_10,
                            20 * rush_td_30,
                            25 * rush_td_50,
                            10 * rush_yds_100,
                            15 * rush_yds_150,
                            25 * rush_yds_200,
                            nw_fpts_combo/2),
         std_fpts_rush = std_fpts_rush) %>% 
  select(-nw_fpts_combo) %>% 
  mutate(across(where(is.numeric), round, 2))




