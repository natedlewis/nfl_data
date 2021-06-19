# prepare data ----

# load pbp
future::plan("multisession")
pbp <- nflfastR::load_pbp(2010:2020, qs = TRUE) %>% 
  progressr::with_progress()

options(scipen = 9999)

# filter out dead plays and post season games
outside_rz <- pbp %>%
  dplyr::filter(
    season_type == "REG",
    .data$yardline_100 > 20,
    !is.na(.data$down),
    .data$play_type %in% c("pass", "qb_kneel", "qb_spike", "run")
  ) %>%
  decode_player_ids()

# load weekly rushing stats
outside_rz_rush_stats <- calculate_player_stats(outside_rz %>% filter(.data$play_type %in% c("run", "qb_kneel")), weekly = TRUE)

outside_rz_stats <- calculate_player_stats(outside_rz, weekly = TRUE)

# load roster
roster <- fast_scraper_roster(2010:2020)


# carries/rushing tds
rush <- outside_rz %>%
  dplyr::filter(.data$play_type %in% c("run", "qb_kneel")) %>%
  dplyr::group_by(.data$rusher_player_id, .data$season) %>%
  dplyr::summarize(
    name_rush = dplyr::first(.data$rusher_player_name),
    team_rush = dplyr::first(.data$posteam),
    outside_rz_carries = dplyr::n(),
    "rush_td_1" = sum(.data$td_player_id == .data$rusher_player_id & .data$yards_gained <= 9, na.rm = TRUE),
    "rush_td_10" = sum(.data$td_player_id == .data$rusher_player_id & .data$yards_gained >= 10 & yards_gained <= 29, na.rm = TRUE),
    "rush_td_30" = sum(.data$td_player_id == .data$rusher_player_id & .data$yards_gained >= 30 & yards_gained <= 49, na.rm = TRUE),
    "rush_td_50" = sum(.data$td_player_id == .data$rusher_player_id & .data$yards_gained >= 50, na.rm = TRUE)) %>% 
  rename(player_id = rusher_player_id)

# rushing milestones
rush_ms <- outside_rz_rush_stats %>% 
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
combo_ms <- outside_rz_stats %>% 
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




