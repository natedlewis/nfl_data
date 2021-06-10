# load plays
future::plan("multisession")
pbp <- nflfastR::load_pbp(2010:2020, qs = TRUE) %>% 
  progressr::with_progress()

# load weekly stats
raw_wkly <- calculate_player_stats(pbp, weekly = TRUE)

# built in function more finding the most commonly used player name
custom_mode <- function(x, na.rm = TRUE) {
  if(na.rm){x <- x[!is.na(x)]}
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
rule_header <- function(x) {
  rlang::inform(
    cli::rule(
      left = ifelse(is_installed("crayon"), crayon::bold(x), glue::glue("\033[1m{x}\033[22m")),
      right = paste0("nflfastR version ", utils::packageVersion("nflfastR")),
      width = getOption("width")
    )
  )
}
rule_footer <- function(x) {
  rlang::inform(
    cli::rule(
      left = ifelse(is_installed("crayon"), crayon::bold(x), glue::glue("\033[1m{x}\033[22m")),
      width = getOption("width")
    )
  )
}

# create temp dfs to avoid joining duplicates
y1 <- roster %>% select(-team)
y2 <- ngs_wkly %>% select(-player_name, -team, -position) %>% filter(week != 0)
y3 <- adv_wkly %>% select(-player_name, -recent_team)
y4 <- nw_wkly %>% select(-player_name, -recent_team)
y5 <- inj_wkly %>% select(-full_name, -team_abbr)
y6 <- games %>% select(team, season, week, game_id, team_coach, opp, opp_coach)

# join all additional objects to weekly stats
full_wkly <- raw_wkly %>% 
  left_join(y1, by = c("player_id", "season")) %>% 
  left_join(y2, by = c("player_id", "week", "season")) %>% 
  left_join(y3, by = c("player_id", "week", "season")) %>% 
  left_join(y4, by = c("player_id", "week", "season")) %>% 
  left_join(y5, by = c("player_id", "week", "season")) %>% 
  left_join(y6, by = c("week", "season", "recent_team" = "team"))

# remove stale objects
rm(y1, y2, y3, y4, y5, y6)

# create objects for regular season games
reg_wkly <- full_wkly %>% filter(week <= 17)

# sum full weekly stats to get totals across full season
reg_ovr <- reg_wkly %>%
  dplyr::group_by(.data$player_id, season) %>%
  dplyr::summarise(
    season = last(season),
    player_name = custom_mode(.data$player_name),
    games = dplyr::n(),
    recent_team = dplyr::last(.data$recent_team),
    position = dplyr::last(.data$position),
    age = dplyr::last(.data$age),
    
    # passing
    cmp = sum(.data$completions),
    att = sum(.data$attempts),
    pass_yds = sum(.data$passing_yards),
    pass_tds = sum(.data$passing_tds),
    ints = sum(.data$interceptions),
    sacks = sum(.data$sacks),
    pass_ayds = sum(.data$passing_air_yards),
    pass_yac = sum(.data$passing_yards_after_catch),
    pass_1st = sum(.data$passing_first_downs),
    rz_att = sum(rz_att),
    ez_att = sum(ez_att),
    deep_att = sum(deep_att),
    rz_cmp = sum(rz_cmp),
    ez_cmp = sum(ez_cmp),
    deep_cmp = sum(deep_cmp),
    rz_cmp_rate = ifelse(rz_att >= 30, rz_cmp/rz_att, NA),
    ez_cmp_rate = ifelse(ez_att >= 10, ez_cmp/ez_att, NA),
    deep_cmp_rate = ifelse(deep_att >= 30, deep_cmp/deep_att, NA),
    avg_time_to_throw = mean(avg_time_to_throw, na.rm = TRUE),
    avg_comp_ayds = mean(avg_completed_air_yards, na.rm = TRUE),
    avg_int_ayds = mean(avg_intended_air_yards, na.rm = TRUE),
    avg_ayds_diff = mean(avg_air_yards_differential, na.rm = TRUE),
    agg = mean(aggressiveness, na.rm = TRUE),
    max_cmp_air_dis = mean(max_completed_air_distance, na.rm = TRUE),
    avg_ayds_to_sticks = mean(avg_air_yards_to_sticks, na.rm = TRUE),
    passer_rating = mean(passer_rating, na.rm = TRUE),
    cmp_pct = mean(completion_percentage, na.rm = TRUE),
    max_air_dis = mean(max_air_distance, na.rm = TRUE),
    pass_yds_300 = sum(pass_yds_300),
    pass_yds_350 = sum(pass_yds_350),
    pass_yds_400 = sum(pass_yds_400),
    pass_td_1 = sum(pass_td_1),
    pass_td_20 = sum(pass_td_20),
    pass_td_50 = sum(pass_td_50),
    int_tds = sum(int_tds),
    
    # rushing
    car = sum(.data$carries),
    rush_yds = sum(.data$rushing_yards),
    rush_tds = sum(.data$rushing_tds),
    rush_1st = sum(.data$rushing_first_downs),
    touches = sum(touches),
    big_runs = sum(big_runs),
    in_five_car = sum(in_five_carries),
    in_ten_car = sum(in_ten_carries),
    neu_car = sum(neu_carries),
    eff = mean(efficiency, na.rm = TRUE),
    percent_attempts_gte_eight_defenders = mean(percent_attempts_gte_eight_defenders, na.rm = TRUE),
    ttlos = mean(avg_time_to_los, na.rm = TRUE),
    x_rush_yds = sum(expected_rush_yards),
    roe = mean(rush_pct_over_expected, na.rm = TRUE),
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
    rec = sum(.data$receptions),
    tgts = sum(.data$targets),
    rec_yds = sum(.data$receiving_yards),
    rec_tds = sum(.data$receiving_tds),
    rec_ayds = sum(.data$receiving_air_yards),
    rec_yac = sum(.data$receiving_yards_after_catch),
    rec_1st = sum(.data$receiving_first_downs),
    rz_tgts = sum(rz_tgts),
    ez_tgts = sum(ez_tgts),
    deep_tgts = sum(deep_tgts),
    adot = mean(adot),
    rz_rec = sum(rz_rec),
    ez_rec = sum(ez_rec),
    deep_rec = sum(deep_rec),
    cth_rate = mean(catch_rate),
    rz_cth_rate = ifelse(rz_tgts >= 30, rz_rec/rz_tgts, NA),
    ez_cth_rate = ifelse(ez_tgts >= 10, ez_rec/ez_tgts, NA),
    deep_cth_rate = ifelse(deep_tgts >= 30, deep_rec/deep_tgts, NA),
    cush = mean(avg_cushion, na.rm = TRUE),
    tay_pct = mean(percent_share_of_intended_air_yards, na.rm = TRUE),
    avg_yac = mean(avg_yac, na.rm = TRUE),
    xyac = mean(avg_expected_yac, na.rm = TRUE),
    yac_ab_x = mean(avg_yac_above_expectation, na.rm = TRUE),
    rec_yds_100 = sum(rec_yds_100),
    rec_yds_150 = sum(rec_yds_150),
    rec_yds_200 = sum(rec_yds_200),
    rec_td_1 = sum(rec_td_1),
    rec_td_20 = sum(rec_td_20),
    rec_td_50 = sum(rec_td_50),
    
    # snaps/st
    st_td_1 = sum(st_td_1),
    st_td_10 = sum(st_td_10),
    st_td_30 = sum(st_td_30),
    st_td_50 = sum(st_td_50),
    tot_snaps = sum(total_snaps),
    off_snaps = sum(offense_snaps),
    offense_snap_rate = mean(offense_snap_rate),
    st_snap_rate = mean(special_teams_snap_rate),
    games_act = sum(ifelse(active_inactive == "Active",1,0)),
    
    # fantasy
    nw_fpts = sum(nw_fantasy_points),
    sd_nw = sd(nw_fantasy_points),
    "nw_fpts/g" = nw_fpts/games,
    fpts = sum(fantasy_points),
    ) %>% 
  arrange(-nw_fpts) %>% 
  dplyr::group_by(position, season) %>%
  dplyr::mutate(nw_pos_rk = 1:n()) %>%
  arrange(-fpts) %>% 
  dplyr::mutate(std_pos_rk = 1:n()) %>%
  ungroup()

#qb stats
ovr_qb <- reg_ovr %>% 
  filter(position == "QB") %>% 
  select(player_id, player_name, recent_team, position, age,
         cmp:int_tds,
         car:rush_tds, in_five_car,
         nw_fpts:std_pos_rk) %>% 
  relocate(nw_pos_rk, .after = age) %>% 
  mutate(across(where(is.numeric), round, 2))

# wr/te stats
ovr_wr_te <- reg_ovr %>% 
  filter(position %in% c("WR", "TE")) %>% 
  select(player_id, player_name, recent_team, position, age,
         rec:rec_td_50,
         nw_fpts:std_pos_rk) %>% 
  relocate(nw_pos_rk, .after = age) %>% 
  mutate(across(where(is.numeric), round, 2))

# rb stats
ovr_rb <- reg_ovr %>% 
  filter(position == "RB") %>% 
  select(player_id, player_name, recent_team, position, age,
         car:combo_yds_150,
         rec:rec_tds, rz_tgts,
         nw_fpts:std_pos_rk) %>% 
  relocate(nw_pos_rk, .after = age) %>% 
  mutate(across(where(is.numeric), round, 2))

# wr/te weekly stats
wr_wkly <- full_wkly %>% 
  filter(position %in% c("WR", "TE"), season == 2020, week <= 17) %>% 
  dplyr::select(tidyselect::any_of(c(
    
    # id information
    "player_id", "full_name", "recent_team", "position", "season", "week",
    
    # receiving stats
    "receptions", "targets", "receiving_yards", "receiving_tds", "receiving_fumbles",
    "receiving_fumbles_lost", "receiving_air_yards", "receiving_yards_after_catch",
    "receiving_first_downs", "receiving_epa", "receiving_2pt_conversions",
    
    # adv/nw stats
    "rz_tgts", "ez_tgts", "deep_tgts", "rz_rec", "ez_rec",
    "deep_rec",  "catch_rate", "adot", "rz_rec_rate", "ez_rec_rate", "deep_rec_rate",
    "rec_td_1", "rec_td_20", "rec_td_50", "nw_fantasy_points", "fantasy_points", "fantasy_points_ppr",
    
    # nsg stats
    "avg_intented_air_yards", "avg_cushion", "percent_share_of_intended_air_yards", "avg_yac", "avg_expected_yac",
    "avg_yac_above_expectation",
    
    # injury status
    "active_inactive", "game_designation", "injury_type", "started",
    
    # snap counts
    "total_snaps", "offense_snaps", "offense_snap_rate", "special_teams_snaps", "special_teams_snap_rate"
  )))

