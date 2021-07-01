# Calcultae player stats with additional variables
# pbp <- load_pbp(2010:2020) %>% filter(season_type == "REG")

options(dplyr.summarise.inform = FALSE)

custom_mode <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
rule_header <- function(x) {
  rlang::inform(cli::rule(
    left = ifelse(
      is_installed("crayon"),
      crayon::bold(x),
      glue::glue("\033[1m{x}\033[22m")
    ),
    right = paste0("nflfastR version ", utils::packageVersion("nflfastR")),
    width = getOption("width")
  ))
}
rule_footer <- function(x) {
  rlang::inform(cli::rule(
    left = ifelse(
      is_installed("crayon"),
      crayon::bold(x),
      glue::glue("\033[1m{x}\033[22m")
    ),
    width = getOption("width")
  ))
}
add_dakota <- function(add_to_this, pbp, weekly) {
  dakota_model <- NULL
  con <-
    url(
      "https://github.com/nflverse/nflfastR-data/blob/master/models/dakota_model.Rdata?raw=true"
    )
  try(load(con), silent = TRUE)
  close(con)
  
  if (is.null(dakota_model)) {
    user_message(
      "This function needs to download the model data from GitHub. Please check your Internet connection and try again!",
      "oops"
    )
    return(add_to_this)
  }
  
  if (!"id" %in% names(pbp))
    pbp <- clean_pbp(pbp)
  if (!"qb_epa" %in% names(pbp))
    pbp <- add_qb_epa(pbp)
  
  suppressMessages({
    df <- pbp %>%
      dplyr::filter(.data$pass == 1 | .data$rush == 1) %>%
      dplyr::filter(
        !is.na(.data$posteam) &
          !is.na(.data$qb_epa) & !is.na(.data$id) & !is.na(.data$down)
      ) %>%
      dplyr::mutate(epa = dplyr::if_else(.data$qb_epa < -4.5,-4.5, .data$qb_epa)) %>%
      decode_player_ids()
  })
  
  if (isTRUE(weekly)) {
    relevant_players <- add_to_this %>%
      dplyr::filter(.data$attempts >= 5) %>%
      dplyr::mutate(filter_id = paste(.data$player_id, .data$season, .data$week, sep = "_")) %>%
      dplyr::pull(.data$filter_id)
    
    model_data <- df %>%
      dplyr::group_by(.data$id, .data$week, .data$season) %>%
      dplyr::summarize(
        n_plays = n(),
        epa_per_play = sum(.data$epa) / .data$n_plays,
        cpoe = mean(.data$cpoe, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(cpoe = dplyr::if_else(is.na(.data$cpoe), 0, .data$cpoe)) %>%
      dplyr::rename(player_id = .data$id) %>%
      dplyr::mutate(filter_id = paste(.data$player_id, .data$season, .data$week, sep = "_")) %>%
      dplyr::filter(.data$filter_id %in% relevant_players)
    
    model_data$dakota <-
      mgcv::predict.gam(dakota_model, model_data) %>% as.vector()
    
    out <- add_to_this %>%
      dplyr::left_join(
        model_data %>%
          dplyr::select(.data$player_id, .data$week, .data$season, .data$dakota),
        by = c("player_id", "week", "season")
      )
  } else if (isFALSE(weekly)) {
    relevant_players <- add_to_this %>%
      dplyr::filter(.data$attempts >= 5) %>%
      dplyr::pull(.data$player_id)
    
    model_data <- df %>%
      dplyr::group_by(.data$id) %>%
      dplyr::summarize(
        n_plays = n(),
        epa_per_play = sum(.data$epa) / .data$n_plays,
        cpoe = mean(.data$cpoe, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(cpoe = dplyr::if_else(is.na(.data$cpoe), 0, .data$cpoe)) %>%
      dplyr::rename(player_id = .data$id) %>%
      dplyr::filter(.data$player_id %in% relevant_players)
    
    model_data$dakota <-
      mgcv::predict.gam(dakota_model, model_data) %>% as.vector()
    
    out <- add_to_this %>%
      dplyr::left_join(model_data %>%
                         dplyr::select(.data$player_id, .data$dakota),
                       by = "player_id")
  }
  return(out)
}
calculate_full_stats <- function(pbp, weekly = FALSE) {
  
  
  # Prepare data ------------------------------------------------------------
  
  # load plays with multiple laterals
  con <- url("https://github.com/mrcaseb/nfl-data/blob/master/data/lateral_yards/multiple_lateral_yards.rds?raw=true")
  mult_lats <- readRDS(con) %>%
    dplyr::mutate(
      season = substr(.data$game_id, 1, 4) %>% as.integer(),
      week = substr(.data$game_id, 6, 7) %>% as.integer()
    ) %>%
    dplyr::filter(.data$yards != 0) %>%
    # the list includes all plays with multiple laterals
    # and all receivers. Since the last one already is in the
    # pbp data, we have to drop him here so the entry isn't duplicated
    dplyr::group_by(.data$game_id, .data$play_id) %>%
    dplyr::slice(seq_len(dplyr::n() - 1)) %>%
    dplyr::ungroup()
  close(con)
  
  # filter down to the 2 dfs we need
  suppressMessages({
    # 1. for "normal" plays: get plays that count in official stats
    data <- pbp %>%
      dplyr::filter(
        !is.na(.data$down),
        .data$play_type %in% c("pass", "qb_kneel", "qb_spike", "run")
      ) %>%
      decode_player_ids()
    
    if (!"qb_epa" %in% names(data)) data <- add_qb_epa(data)
    
    # 2. for 2pt conversions only, get those plays
    two_points <- pbp %>%
      dplyr::filter(.data$two_point_conv_result == "success") %>%
      dplyr::select(
        "week", "season", "posteam",
        "pass_attempt", "rush_attempt",
        "passer_player_name", "passer_player_id",
        "rusher_player_name", "rusher_player_id",
        "lateral_rusher_player_name", "lateral_rusher_player_id",
        "receiver_player_name", "receiver_player_id",
        "lateral_receiver_player_name", "lateral_receiver_player_id"
      ) %>%
      decode_player_ids()
  })
  
  if (!"special" %in% names(pbp)) {# we need this column for the special teams tds
    pbp <- pbp %>%
      dplyr::mutate(
        special = dplyr::if_else(
          .data$play_type %in% c("extra_point","field_goal","kickoff","punt"),
          1, 0
        )
      )
  }
  
  s_type <- pbp %>%
    dplyr::select(.data$season, .data$season_type, .data$week) %>%
    dplyr::distinct()
  
  # Passing stats -----------------------------------------------------------
  
  # get passing stats
  pass_df <- data %>%
    dplyr::filter(.data$play_type %in% c("pass", "qb_spike")) %>%
    dplyr::group_by(.data$passer_player_id, .data$week, .data$season) %>%
    dplyr::summarize(
      passing_yards_after_catch = sum((.data$passing_yards - .data$air_yards) * .data$complete_pass, na.rm = TRUE),
      name_pass = dplyr::first(.data$passer_player_name),
      team_pass = dplyr::first(.data$posteam),
      passing_yards = sum(.data$passing_yards, na.rm = TRUE),
      passing_tds = sum(.data$touchdown == 1 & .data$td_team == .data$posteam & .data$complete_pass == 1),
      interceptions = sum(.data$interception),
      attempts = sum(.data$complete_pass == 1 | .data$incomplete_pass == 1 | .data$interception == 1),
      completions = sum(.data$complete_pass == 1),
      sack_fumbles = sum(.data$fumble == 1 & .data$fumbled_1_player_id == .data$passer_player_id),
      sack_fumbles_lost = sum(.data$fumble_lost == 1 & .data$fumbled_1_player_id == .data$passer_player_id),
      passing_air_yards = sum(.data$air_yards, na.rm = TRUE),
      sacks = sum(.data$sack),
      sack_yards = -1*sum(.data$yards_gained * .data$sack),
      passing_first_downs = sum(.data$first_down_pass),
      passing_epa = sum(.data$qb_epa, na.rm = TRUE),
      outside_rz_attempts = sum(.data$attempts &
                                  .data$yardline_100 >= 20, na.rm = TRUE),
      rz_attempts = sum(.data$attempts &
                          .data$yardline_100 < 20, na.rm = TRUE),
      ez_attempts = sum(
        .data$attempts &
          .data$yardline_100 == .data$air_yards,
        na.rm = TRUE
      ),
      ez_attempts_inside_rz = sum(
        .data$attempts &
          .data$yardline_100 == .data$air_yards &
          .data$yardline_100 < 20,
        na.rm = TRUE
      ),
      ez_attempts_outside_rz = sum(
        .data$attempts &
          .data$yardline_100 == .data$air_yards &
          .data$yardline_100 >= 20,
        na.rm = TRUE
      ),
      deep_attempts = sum(.data$attempts &
                            .data$air_yards >= 20, na.rm = TRUE),
      short_attempts = sum(.data$attempts &
                             .data$air_yards <= 5, na.rm = TRUE),
      neutral_attempts = sum(
        attempts &
          .data$wp > .20 &
          .data$wp < .80 &
          .data$down <= 2 &
          .data$qtr <= 2 & .data$half_seconds_remaining > 120,
        na.rm = TRUE
      ),
      garbadge_time_attempts = sum(attempts &
                                     .data$wp >= .95 |
                                     .data$wp <= 0.05, na.rm = TRUE),
      rz_completions = sum(.data$completions &
                             yardline_100 < 20, na.rm = TRUE),
      ez_completions = sum(
        .data$completions &
          .data$yardline_100 == .data$air_yards,
        na.rm = TRUE
      ),
      ez_completions_inside_rz = sum(
        .data$completions &
          .data$yardline_100 == .data$air_yards &
          yardline_100 < 20,
        na.rm = TRUE
      ),
      ez_completions_outside_rz = sum(
        .data$completions &
          .data$yardline_100 == .data$air_yards &
          yardline_100 >= 20,
        na.rm = TRUE
      ),
      deep_completions = sum(.data$completions &
                               .data$air_yards >= 20, na.rm = TRUE),
      short_completions = sum(.data$completions &
                                .data$air_yards <= 5, na.rm = TRUE),
      rz_completion_pct = ifelse(rz_attempts >= 1, rz_completions / rz_attempts, NA),
      ez_completion_pct = ifelse(ez_attempts >= 1, ez_completions / ez_attempts, NA),
      deep_completion_pct = ifelse(deep_attempts >= 1, deep_completions / deep_attempts, NA),
      short_completion_pct = ifelse(short_attempts >= 1, short_completions / short_attempts, NA),
      "passing_tds_1" = sum(
        .data$touchdown == 1 &
          .data$td_team == .data$posteam &
          .data$complete_pass == 1 & .data$yards_gained <= 19
      ),
      "passing_tds_20" = sum(
        .data$touchdown == 1 &
          .data$td_team == .data$posteam &
          .data$complete_pass == 1 &
          .data$yards_gained >= 20 & .data$yards_gained <= 49
      ),
      "passing_tds_50" = sum(
        .data$touchdown == 1 &
          .data$td_team == .data$posteam &
          .data$complete_pass == 1 & .data$yards_gained >= 50
      ),
      "passing_yards_300" = sum(ifelse(
        passing_yards >= 300 &
          passing_yards < 350, 1, 0
      )),
      "passing_yards_350" = sum(ifelse(
        passing_yards >= 350 &
          passing_yards < 400, 1, 0
      )),
      "passing_yards_400" = sum(ifelse(passing_yards >= 400, 1, 0)),
      int_tds = sum(
        .data$interception &
          .data$touchdown == 1 & .data$td_team == .data$defteam)
    ) %>%
    dplyr::rename(player_id = .data$passer_player_id) %>%
    dplyr::ungroup()
  
  if (isTRUE(weekly)) pass_df <- add_dakota(pass_df, pbp = pbp, weekly = weekly)
  
  pass_two_points <- two_points %>%
    dplyr::filter(.data$pass_attempt == 1) %>%
    dplyr::group_by(.data$passer_player_id, .data$week, .data$season) %>%
    dplyr::summarise(
      # need name_pass and team_pass here for the full join in the next pipe
      name_pass = custom_mode(.data$passer_player_name),
      team_pass = custom_mode(.data$posteam),
      passing_2pt_conversions = dplyr::n()
    ) %>%
    dplyr::rename(player_id = .data$passer_player_id) %>%
    dplyr::ungroup()
  
  pass_df <- pass_df %>%
    # need a full join because players without passing stats that recorded
    # a passing two point (e.g. WRs) are dropped in any other join
    dplyr::full_join(pass_two_points, by = c("player_id", "week", "season", "name_pass", "team_pass")) %>%
    dplyr::mutate(passing_2pt_conversions = dplyr::if_else(is.na(.data$passing_2pt_conversions), 0L, .data$passing_2pt_conversions)) %>%
    dplyr::filter(!is.na(.data$player_id))
  
  pass_df_nas <- is.na(pass_df)
  epa_index <- which(dimnames(pass_df_nas)[[2]] %in% c("passing_epa", "dakota"))
  pass_df_nas[,epa_index] <- c(FALSE)
  
  pass_df[pass_df_nas] <- 0
  
  # Rushing stats -----------------------------------------------------------
  
  # rush df 1: primary rusher
  rushes <- data %>%
    dplyr::filter(.data$play_type %in% c("run", "qb_kneel")) %>%
    dplyr::group_by(.data$rusher_player_id, .data$week, .data$season) %>%
    dplyr::summarize(
      name_rush = dplyr::first(.data$rusher_player_name),
      team_rush = dplyr::first(.data$posteam),
      yards = sum(.data$rushing_yards, na.rm = TRUE),
      tds = sum(.data$td_player_id == .data$rusher_player_id, na.rm = TRUE),
      carries = dplyr::n(),
      rushing_fumbles = sum(.data$fumble == 1 & .data$fumbled_1_player_id == .data$rusher_player_id & is.na(.data$lateral_rusher_player_id)),
      rushing_fumbles_lost = sum(.data$fumble_lost == 1 & .data$fumbled_1_player_id == .data$rusher_player_id & is.na(.data$lateral_rusher_player_id)),
      rushing_first_downs = sum(.data$first_down_rush & is.na(.data$lateral_rusher_player_id)),
      rushing_epa = sum(.data$epa, na.rm = TRUE),
      rz_carries = sum(carries &
                         .data$yardline_100 < 20, na.rm = TRUE),
      outside_rz_carries = sum(carries &
                                 .data$yardline_100 >= 20, na.rm = TRUE),
      inside_five_carries = sum(carries &
                                  .data$yardline_100 < 5, na.rm = TRUE),
      inside_ten_carries = sum(carries &
                                 .data$yardline_100 < 10, na.rm = TRUE),
      neutral_carries =
        sum(
          carries &
            .data$wp > .20 &
            .data$wp < .80 &
            .data$down <= 2 &
            .data$qtr <= 2 &
            .data$half_seconds_remaining > 120,
          na.rm = TRUE
        ),
      garbadge_time_carries =
        sum(carries &
              .data$wp >= .95 |
              .data$wp <= 0.05,
            na.rm = TRUE),
      "rushing_tds_1" = sum(
        .data$td_player_id == .data$rusher_player_id &
          .data$yards_gained <= 9,
        na.rm = TRUE
      ),
      "rushing_tds_10" = sum(
        .data$td_player_id == .data$rusher_player_id &
          .data$yards_gained >= 10 & yards_gained <= 29,
        na.rm = TRUE
      ),
      "rushing_tds_30" = sum(
        .data$td_player_id == .data$rusher_player_id &
          .data$yards_gained >= 30 & yards_gained <= 49,
        na.rm = TRUE
      ),
      "rushing_tds_50" = sum(
        .data$td_player_id == .data$rusher_player_id &
          .data$yards_gained >= 50,
        na.rm = TRUE
      )
    ) %>%
    dplyr::ungroup()
  
  # rush df 2: lateral
  laterals <- data %>%
    dplyr::filter(!is.na(.data$lateral_rusher_player_id)) %>%
    dplyr::group_by(.data$lateral_rusher_player_id, .data$week, .data$season) %>%
    dplyr::summarize(
      lateral_yards = sum(.data$lateral_rushing_yards, na.rm = TRUE),
      lateral_fds = sum(.data$first_down_rush, na.rm = TRUE),
      lateral_tds = sum(.data$td_player_id == .data$lateral_rusher_player_id, na.rm = TRUE),
      lateral_att = dplyr::n(),
      lateral_fumbles =  sum(.data$fumble, na.rm = TRUE),
      lateral_fumbles_lost = sum(.data$fumble_lost, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(rusher_player_id = .data$lateral_rusher_player_id) %>%
    dplyr::bind_rows(
      mult_lats %>%
        dplyr::filter(
          .data$type == "lateral_rushing" & .data$season %in% data$season & .data$week %in% data$week
        ) %>%
        dplyr::select("season", "week", "rusher_player_id" = .data$gsis_player_id, "lateral_yards" = .data$yards) %>%
        dplyr::mutate(lateral_tds = 0L, lateral_att = 1L)
    )
  
  # rush df: join
  rush_df <- rushes %>%
    dplyr::left_join(laterals, by = c("rusher_player_id", "week", "season")) %>%
    dplyr::mutate(
      lateral_yards = dplyr::if_else(is.na(.data$lateral_yards), 0, .data$lateral_yards),
      lateral_tds = dplyr::if_else(is.na(.data$lateral_tds), 0L, .data$lateral_tds),
      lateral_fumbles = dplyr::if_else(is.na(.data$lateral_fumbles), 0, .data$lateral_fumbles),
      lateral_fumbles_lost = dplyr::if_else(is.na(.data$lateral_fumbles_lost), 0, .data$lateral_fumbles_lost),
      lateral_fds = dplyr::if_else(is.na(.data$lateral_fds), 0, .data$lateral_fds)
    ) %>%
    dplyr::mutate(
      rushing_yards = .data$yards + .data$lateral_yards,
      rushing_tds = .data$tds + .data$lateral_tds,
      rushing_first_downs = .data$rushing_first_downs + .data$lateral_fds,
      rushing_fumbles = .data$rushing_fumbles + .data$lateral_fumbles,
      rushing_fumbles_lost = .data$rushing_fumbles_lost + .data$lateral_fumbles_lost,
      "rushing_yards_100" = ifelse(rushing_yards >= 100 &
                                     rushing_yards < 150, 1, 0),
      "rushing_yards_150" = ifelse(rushing_yards >= 150 &
                                     rushing_yards < 200, 1, 0),
      "rushing_yards_200" = ifelse(rushing_yards >= 200, 1, 0)
    ) %>%
    dplyr::rename(player_id = .data$rusher_player_id) %>%
    dplyr::select("player_id", "week", "season", "name_rush", "team_rush",
                  "rushing_yards", "carries", "rushing_tds", "rushing_fumbles",
                  "rushing_fumbles_lost", "rushing_first_downs", "rushing_epa",
                  "outside_rz_carries",
                  "rz_carries",
                  "inside_ten_carries",
                  "inside_five_carries",
                  "neutral_carries",
                  "garbadge_time_carries",
                  "rushing_tds_1",
                  "rushing_tds_10",
                  "rushing_tds_30",
                  "rushing_tds_50",
                  "rushing_yards_100",
                  "rushing_yards_150",
                  "rushing_yards_200") %>%
    dplyr::ungroup()
  
  rush_two_points <- two_points %>%
    dplyr::filter(.data$rush_attempt == 1) %>%
    dplyr::group_by(.data$rusher_player_id, .data$week, .data$season) %>%
    dplyr::summarise(
      # need name_rush and team_rush here for the full join in the next pipe
      name_rush = custom_mode(.data$rusher_player_name),
      team_rush = custom_mode(.data$posteam),
      rushing_2pt_conversions = dplyr::n()
    ) %>%
    dplyr::rename(player_id = .data$rusher_player_id) %>%
    dplyr::ungroup()
  
  rush_df <- rush_df %>%
    # need a full join because players without rushing stats that recorded
    # a rushing two point (mostly QBs) are dropped in any other join
    dplyr::full_join(rush_two_points, by = c("player_id", "week", "season", "name_rush", "team_rush")) %>%
    dplyr::mutate(rushing_2pt_conversions = dplyr::if_else(is.na(.data$rushing_2pt_conversions), 0L, .data$rushing_2pt_conversions)) %>%
    dplyr::filter(!is.na(.data$player_id))
  
  rush_df_nas <- is.na(rush_df)
  epa_index <- which(dimnames(rush_df_nas)[[2]] == "rushing_epa")
  rush_df_nas[,epa_index] <- c(FALSE)
  
  rush_df[rush_df_nas] <- 0
  
  # Receiving stats ---------------------------------------------------------
  
  # receiver df 1: primary receiver
  rec <- data %>%
    dplyr::filter(!is.na(.data$receiver_player_id)) %>%
    dplyr::group_by(.data$receiver_player_id, .data$week, .data$season) %>%
    dplyr::summarize(
      name_receiver = dplyr::first(.data$receiver_player_name),
      team_receiver = dplyr::first(.data$posteam),
      yards = sum(.data$receiving_yards, na.rm = TRUE),
      receptions = sum(.data$complete_pass == 1),
      targets = dplyr::n(),
      tds = sum(.data$td_player_id == .data$receiver_player_id, na.rm = TRUE),
      receiving_fumbles = sum(.data$fumble == 1 & .data$fumbled_1_player_id == .data$receiver_player_id & is.na(.data$lateral_receiver_player_id)),
      receiving_fumbles_lost = sum(.data$fumble_lost == 1 & .data$fumbled_1_player_id == .data$receiver_player_id & is.na(.data$lateral_receiver_player_id)),
      receiving_air_yards = sum(.data$air_yards, na.rm = TRUE),
      receiving_yards_after_catch = sum(.data$yards_after_catch, na.rm = TRUE),
      receiving_first_downs = sum(.data$first_down_pass & is.na(.data$lateral_receiver_player_id)),
      receiving_epa = sum(.data$epa, na.rm = TRUE),
      rz_receptions = sum(.data$complete_pass == 1 &
                            yardline_100 < 20, na.rm = TRUE),
      ez_receptions = sum(
        .data$complete_pass == 1 &
          .data$yardline_100 == .data$air_yards,
        na.rm = TRUE
      ),
      ez_receptions_inside_rz = sum(
        .data$complete_pass == 1 &
          .data$yardline_100 == .data$air_yards &
          yardline_100 < 20,
        na.rm = TRUE
      ),
      ez_receptions_outside_rz = sum(
        .data$complete_pass == 1 &
          .data$yardline_100 == .data$air_yards &
          yardline_100 >= 20,
        na.rm = TRUE
      ),
      deep_receptions = sum(.data$complete_pass == 1 &
                              .data$air_yards >= 20, na.rm = TRUE),
      short_receptions = sum(.data$complete_pass == 1 &
                               .data$air_yards <= 5, na.rm = TRUE),
      rz_targets = sum(.data$targets &
                         yardline_100 < 20, na.rm = TRUE),
      outside_rz_targets = sum(.data$targets &
                                 yardline_100 >= 20, na.rm = TRUE),
      ez_targets = sum(.data$targets &
                         .data$yardline_100 == .data$air_yards, na.rm = TRUE),
      ez_targets_inside_rz = sum(
        .data$targets &
          .data$yardline_100 == .data$air_yards &
          yardline_100 < 20,
        na.rm = TRUE
      ),
      ez_targets_outside_rz = sum(
        .data$targets &
          .data$yardline_100 == .data$air_yards &
          yardline_100 >= 20,
        na.rm = TRUE
      ),
      deep_targets = sum(.data$targets &
                           .data$air_yards >= 20, na.rm = TRUE),
      short_targets = sum(.data$targets &
                            .data$air_yards <= 5, na.rm = TRUE),
      neutral_targets = sum(
        targets &
          .data$wp > .20 &
          .data$wp < .80 &
          .data$down <= 2 &
          .data$qtr <= 2 & .data$half_seconds_remaining > 120,
        na.rm = TRUE
      ),
      garbadge_time_targets = sum(targets &
                                    .data$wp >= .95 |
                                    .data$wp <= 0.05, na.rm = TRUE),
      adot = ifelse(targets >= 1, receiving_air_yards / targets, NA),
      catch_rate = ifelse(targets >= 1, receptions / targets, NA),
      rz_catch_rate = ifelse(rz_targets >= 1, rz_receptions / rz_targets, NA),
      ez_catch_rate = ifelse(ez_targets >= 1, ez_receptions / ez_targets, NA),
      deep_catch_rate = ifelse(deep_targets >= 1, deep_receptions / deep_targets, NA),
      short_catch_rate = ifelse(short_targets >= 1, short_receptions / short_targets, NA),
      "receiving_tds_1" = sum(.data$td_player_id == .data$receiver_player_id &
                                yards_gained <= 19,
                              na.rm = TRUE),
      "receiving_tds_20" = sum(
        .data$td_player_id == .data$receiver_player_id &
          yards_gained >= 20 & yards_gained <= 49,
        na.rm = TRUE
      ),
      "receiving_tds_50" = sum(.data$td_player_id == .data$receiver_player_id &
                                 yards_gained >= 50,
                               na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
  
  # receiver df 2: lateral
  laterals <- data %>%
    dplyr::filter(!is.na(.data$lateral_receiver_player_id)) %>%
    dplyr::group_by(.data$lateral_receiver_player_id, .data$week, .data$season) %>%
    dplyr::summarize(
      lateral_yards = sum(.data$lateral_receiving_yards, na.rm = TRUE),
      lateral_tds = sum(.data$td_player_id == .data$lateral_receiver_player_id, na.rm = TRUE),
      lateral_att = dplyr::n(),
      lateral_fds = sum(.data$first_down_pass, na.rm = T),
      lateral_fumbles = sum(.data$fumble, na.rm = T),
      lateral_fumbles_lost = sum(.data$fumble_lost, na.rm = T)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(receiver_player_id = .data$lateral_receiver_player_id) %>%
    dplyr::bind_rows(
      mult_lats %>%
        dplyr::filter(
          .data$type == "lateral_receiving" & .data$season %in% data$season & .data$week %in% data$week
        ) %>%
        dplyr::select("season", "week", "receiver_player_id" = .data$gsis_player_id, "lateral_yards" = .data$yards) %>%
        dplyr::mutate(lateral_tds = 0L, lateral_att = 1L)
    )
  
  # rec df: join
  rec_df <- rec %>%
    dplyr::left_join(laterals, by = c("receiver_player_id", "week", "season")) %>%
    dplyr::mutate(
      lateral_yards = dplyr::if_else(is.na(.data$lateral_yards), 0, .data$lateral_yards),
      lateral_tds = dplyr::if_else(is.na(.data$lateral_tds), 0L, .data$lateral_tds),
      lateral_fumbles = dplyr::if_else(is.na(.data$lateral_fumbles), 0, .data$lateral_fumbles),
      lateral_fumbles_lost = dplyr::if_else(is.na(.data$lateral_fumbles_lost), 0, .data$lateral_fumbles_lost),
      lateral_fds = dplyr::if_else(is.na(.data$lateral_fds), 0, .data$lateral_fds)
    ) %>%
    dplyr::mutate(
      receiving_yards = .data$yards + .data$lateral_yards,
      receiving_tds = .data$tds + .data$lateral_tds,
      receiving_yards_after_catch = .data$receiving_yards_after_catch + .data$lateral_yards,
      receiving_first_downs = .data$receiving_first_downs + .data$lateral_fds,
      receiving_fumbles = .data$receiving_fumbles + .data$lateral_fumbles,
      receiving_fumbles_lost = .data$receiving_fumbles_lost + .data$lateral_fumbles_lost,
      "receiving_yards_100" = ifelse(receiving_yards >= 100 &
                                       receiving_yards < 150, 1, 0),
      "receiving_yards_150" = ifelse(receiving_yards >= 150 &
                                       receiving_yards < 200, 1, 0),
      "receiving_yards_200" = ifelse(receiving_yards >= 200, 1, 0)
    ) %>%
    dplyr::rename(player_id = .data$receiver_player_id) %>%
    dplyr::select("player_id", "week", "season", "name_receiver", "team_receiver",
                  "receiving_yards", "receiving_air_yards", "receiving_yards_after_catch",
                  "receptions", "targets", "receiving_tds", "receiving_fumbles",
                  "receiving_fumbles_lost", "receiving_first_downs", "receiving_epa",
                  "outside_rz_targets",
                  "rz_targets",
                  "ez_targets",
                  "ez_targets_inside_rz",
                  "ez_targets_outside_rz",
                  "deep_targets",
                  "short_targets",
                  "neutral_targets",
                  "garbadge_time_targets",
                  "catch_rate",
                  "adot",
                  "rz_catch_rate",
                  "ez_catch_rate",
                  "deep_catch_rate",
                  "short_catch_rate",
                  "receiving_yards_100",
                  "receiving_yards_150",
                  "receiving_yards_200",
                  "receiving_tds_1",
                  "receiving_tds_20",
                  "receiving_tds_50")
  
  rec_two_points <- two_points %>%
    dplyr::filter(.data$pass_attempt == 1) %>%
    dplyr::group_by(.data$receiver_player_id, .data$week, .data$season) %>%
    dplyr::summarise(
      # need name_receiver and team_receiver here for the full join in the next pipe
      name_receiver = custom_mode(.data$receiver_player_name),
      team_receiver = custom_mode(.data$posteam),
      receiving_2pt_conversions = dplyr::n()
    ) %>%
    dplyr::rename(player_id = .data$receiver_player_id) %>%
    dplyr::ungroup()
  
  rec_df <- rec_df %>%
    # need a full join because players without receiving stats that recorded
    # a receiving two point are dropped in any other join
    dplyr::full_join(rec_two_points, by = c("player_id", "week", "season", "name_receiver", "team_receiver")) %>%
    dplyr::mutate(receiving_2pt_conversions = dplyr::if_else(is.na(.data$receiving_2pt_conversions), 0L, .data$receiving_2pt_conversions)) %>%
    dplyr::filter(!is.na(.data$player_id))
  
  rec_df_nas <- is.na(rec_df)
  epa_index <- which(dimnames(rec_df_nas)[[2]] == "receiving_epa")
  rec_df_nas[,epa_index] <- c(FALSE)
  
  rec_df[rec_df_nas] <- 0
  
  
  # Special teams -----------------------------------------------------------
  
  st_tds <- pbp %>%
    dplyr::filter(.data$special == 1 & !is.na(.data$td_player_id)) %>%
    dplyr::group_by(.data$td_player_id, .data$week, .data$season) %>%
    dplyr::summarise(
      name_st = custom_mode(.data$td_player_name),
      team_st = custom_mode(.data$td_team),
      special_teams_tds = sum(.data$touchdown, na.rm = TRUE),
      "special_teams_tds_1" = sum(.data$touchdown &
                                    return_yards <= 9, na.rm = TRUE),
      "special_teams_tds_10" = sum(.data$touchdown &
                                     return_yards >= 10 &
                                     return_yards <= 29, na.rm = TRUE),
      "special_teams_tds_30" = sum(.data$touchdown &
                                     return_yards >= 30 &
                                     return_yards <= 49, na.rm = TRUE),
      "special_teams_tds_50" = sum(.data$touchdown &
                                     return_yards >= 50, na.rm = TRUE)
    ) %>%
    dplyr::rename(player_id = .data$td_player_id)
  
  # Combine all stats -------------------------------------------------------
  
  # combine all the stats together
  player_df <- pass_df %>%
    dplyr::full_join(rush_df, by = c("player_id", "week", "season")) %>%
    dplyr::full_join(rec_df, by = c("player_id", "week", "season")) %>%
    dplyr::full_join(st_tds, by = c("player_id", "week", "season")) %>%
    dplyr::left_join(s_type, by = c("season", "week")) %>%
    dplyr::mutate(
      player_name = dplyr::case_when(
        !is.na(.data$name_pass) ~ .data$name_pass,
        !is.na(.data$name_rush) ~ .data$name_rush,
        !is.na(.data$name_receiver) ~ .data$name_receiver,
        TRUE ~ .data$name_st
      ),
      recent_team = dplyr::case_when(
        !is.na(.data$team_pass) ~ .data$team_pass,
        !is.na(.data$team_rush) ~ .data$team_rush,
        !is.na(.data$team_receiver) ~ .data$team_receiver,
        TRUE ~ .data$team_st
      )
    ) %>%
    dplyr::select(any_of(c(
      # id information
      "player_id",
      "player_name",
      "recent_team",
      "season",
      "week",

      # passing stats
      "completions",
      "attempts",
      "passing_yards",
      "passing_tds",
      "interceptions",
      "sacks",
      "sack_yards",
      "sack_fumbles",
      "sack_fumbles_lost",
      "passing_air_yards",
      "passing_yards_after_catch",
      "passing_first_downs",
      "passing_epa",
      "passing_2pt_conversions",
      "dakota",
      "outside_rz_attempts",
      "rz_attempts",
      "ez_attempts",
      "ez_attempts_inside_rz",
      "ez_attempts_outside_rz",
      "deep_attempts",
      "short_attempts",
      "neutral_attempts",
      "garbadge_time_attempts",
      "rz_completion_pct",
      "ez_completion_pct",
      "deep_completion_pct",
      "short_completion_pct",
      "passing_yards_300",
      "passing_yards_350",
      "passing_yards_400",
      "passing_tds_1",
      "passing_tds_20",
      "passing_tds_50",
      "int_tds",
      
      # rushing stats
      "carries",
      "rushing_yards",
      "rushing_tds",
      "rushing_fumbles",
      "rushing_fumbles_lost",
      "rushing_first_downs",
      "rushing_epa",
      "rushing_2pt_conversions",
      "outside_rz_carries",
      "rz_carries",
      "inside_ten_carries",
      "inside_five_carries",
      "neutral_carries",
      "garbadge_time_carries",
      "rushing_yards_100",
      "rushing_yards_150",
      "rushing_yards_200",
      "rushing_tds_1",
      "rushing_tds_10",
      "rushing_tds_30",
      "rushing_tds_50",
      
      # receiving stats
      "receptions",
      "targets",
      "receiving_yards",
      "receiving_tds",
      "receiving_fumbles",
      "receiving_fumbles_lost",
      "receiving_air_yards",
      "receiving_yards_after_catch",
      "receiving_first_downs",
      "receiving_epa",
      "receiving_2pt_conversions",
      "outside_rz_targets",
      "rz_targets",
      "ez_targets",
      "ez_targets_inside_rz",
      "ez_targets_outside_rz",
      "deep_targets",
      "short_targets",
      "neutral_targets",
      "garbadge_time_targets",
      "catch_rate",
      "adot",
      "rz_catch_rate",
      "ez_catch_rate",
      "deep_catch_rate",
      "short_catch_rate",
      "receiving_yards_100",
      "receiving_yards_150",
      "receiving_yards_200",
      "receiving_tds_1",
      "receiving_tds_20",
      "receiving_tds_50",
      
      # special teams
      "special_teams_tds",
      "special_teams_tds_1",
      "special_teams_tds_10",
      "special_teams_tds_30",
      "special_teams_tds_50"
      
    ))) %>%
    dplyr::filter(!is.na(.data$player_id))
  
  player_df_nas <- is.na(player_df)
  epa_index <- which(dimnames(player_df_nas)[[2]] %in% c("passing_epa", "rushing_epa", "receiving_epa", "dakota"))
  player_df_nas[,epa_index] <- c(FALSE)
  player_df[player_df_nas] <- 0
  player_df <- subset(player_df, !duplicated(subset(player_df, select=c(player_id, season, week))))
  
  player_df <- player_df %>%
    dplyr::mutate(
      touches = receptions + carries,
      opportunites = targets + carries,
      total_yards = passing_yards + rushing_yards + receiving_yards,
      total_tds = passing_tds + rushing_tds + receiving_tds,
      combo_yards_100 = ifelse(rushing_yards >= 50 &
                                 receiving_yards >= 50, 1, 0),
      combo_yards_150 = ifelse(rushing_yards >= 75 &
                                 receiving_yards >= 75, 1, 0),
      fantasy_points =
        1 / 25 * .data$passing_yards +
        4 * .data$passing_tds +
        -2 * .data$interceptions +
        1 / 10 * (.data$rushing_yards + .data$receiving_yards) +
        6 * (.data$rushing_tds + .data$receiving_tds + .data$special_teams_tds) +
        2 * (.data$passing_2pt_conversions + .data$rushing_2pt_conversions + .data$receiving_2pt_conversions) +
        -2 * (.data$sack_fumbles_lost + .data$rushing_fumbles_lost + .data$receiving_fumbles_lost),
      
      fantasy_points_ppr = .data$fantasy_points + .data$receptions,
      pass_pts =
        10 * passing_tds_1 +
        15 * passing_tds_20 +
        20 * passing_tds_50 +
        10 * passing_yards_300 +
        15 * passing_yards_350 +
        25 * passing_yards_400 +
        -5 * interceptions +
        -15 * int_tds +
        3 * passing_2pt_conversions,
      combo_pts =
        10 * combo_yards_100 +
        10 * combo_yards_150,
      rush_pts =
        10 * rushing_tds_1 +
        15 * rushing_tds_10 +
        20 * rushing_tds_30 +
        25 * rushing_tds_50 +
        10 * rushing_yards_100 +
        15 * rushing_yards_150 +
        25 * rushing_yards_200 +
        3 * rushing_2pt_conversions,
      rec_pts =
        10 * receiving_tds_1 +
        15 * receiving_tds_20 +
        20 * receiving_tds_50 +
        10 * receiving_yards_100 +
        15 * receiving_yards_150 +
        25 * receiving_yards_200 +
        3 * receiving_2pt_conversions,
      st_pts =
        10 * special_teams_tds_1 +
        15 * special_teams_tds_10 +
        20 * special_teams_tds_30 +
        25 * special_teams_tds_50,
      nw_pts = pass_pts + rush_pts + rec_pts + st_pts + combo_pts,
      rush_pts = rush_pts + (combo_pts / 2),
      rec_pts = rec_pts + (combo_pts / 2)) %>% 
    arrange(.data$player_id, .data$season, .data$week)
  
  # if user doesn't want week-by-week input, aggregate the whole df
  if (isFALSE(weekly)) {
    player_df <- player_df %>%
      dplyr::group_by(.data$player_id, .data$season) %>%
      dplyr::summarise(
        player_name = custom_mode(.data$player_name),
        recent_team = last(.data$recent_team),
        games = n(),
        
        # passing
        completions = sum(completions),
        attempts = sum(attempts),
        passing_yards = sum(attempts),
        passing_tds = sum(passing_tds),
        interceptions = sum(interceptions),
        sacks = sum(sacks),
        passing_air_yards = sum(passing_air_yards),
        passing_yards_after_catch = sum(passing_yards_after_catch),
        passing_first_downs = sum(passing_first_downs),
        passing_epa = dplyr::if_else(all(is.na(.data$passing_epa)), NA_real_, sum(.data$passing_epa, na.rm = TRUE)),
        outside_rz_attempts = sum(outside_rz_attempts),
        rz_attempts = sum(rz_attempts),
        ez_attempts = sum(ez_attempts),
        ez_attempts_inside_rz = sum(ez_attempts_inside_rz),
        ez_attempts_outside_rz = sum(ez_attempts_outside_rz),
        deep_attempts = sum(deep_attempts),
        short_attempts = sum(short_attempts),
        neutral_attempts = sum(neutral_attempts),
        garbadge_time_attempts = sum(garbadge_time_attempts),
        rz_completion_pct = mean(rz_completion_pct),
        ez_completion_pct = mean(ez_completion_pct),
        deep_completion_pct = mean(deep_completion_pct),
        short_completion_pct = mean(short_completion_pct),
        passing_yards_300 = sum(passing_yards_300),
        passing_yards_350 = sum(passing_yards_350),
        passing_yards_400 = sum(passing_yards_350),
        passing_tds_1 = sum(passing_tds_1),
        passing_tds_20 = sum(passing_tds_20),
        passing_tds_50 = sum(passing_tds_50),
        int_tds = sum(int_tds),
        
        # rushing
        carries = sum(carries),
        rushing_yards = sum(rushing_yards),
        rushing_tds = sum(rushing_tds),
        rushing_first_downs = sum(rushing_first_downs),
        rushing_epa = dplyr::if_else(all(is.na(.data$rushing_epa)), NA_real_, sum(.data$rushing_epa, na.rm = TRUE)),
        outside_rz_carries = sum(outside_rz_carries),
        rz_carries = sum(rz_carries),
        inside_ten_carries = sum(inside_ten_carries),
        inside_five_carries = sum(inside_five_carries),
        neutral_carries = sum(neutral_carries),
        garbadge_time_carries = sum(garbadge_time_carries),
        rushing_yards_100 = sum(rushing_yards_100),
        rushing_yards_150 = sum(rushing_yards_150),
        rushing_yards_200 = sum(rushing_yards_200),
        rushing_tds_1 = sum(rushing_tds_1),
        rushing_tds_10 = sum(rushing_tds_10),
        rushing_tds_30 = sum(rushing_tds_30),
        rushing_tds_50 = sum(rushing_tds_50),
        
        # receiving stats
        receptions = sum(receptions),
        targets = sum(targets),
        receiving_yards = sum(receiving_yards),
        receiving_tds = sum(receiving_tds),
        receiving_air_yards = sum(receiving_air_yards),
        receiving_yards_after_catch = sum(receiving_yards_after_catch),
        receiving_first_downs = sum(receiving_first_downs),
        receiving_epa = dplyr::if_else(all(is.na(.data$receiving_epa)), NA_real_, sum(.data$receiving_epa, na.rm = TRUE)),
        outside_rz_targets = sum(outside_rz_targets),
        rz_targets = sum(rz_targets),
        ez_targets = sum(ez_targets),
        ez_targets_inside_rz = sum(ez_targets_inside_rz),
        ez_targets_outside_rz = sum(ez_targets_outside_rz),
        deep_targets = sum(deep_targets),
        short_targets = sum(short_targets),
        neutral_targets = sum(neutral_targets),
        garbadge_time_targets = sum(garbadge_time_targets),
        adot = mean(adot),
        catch_rate = mean(catch_rate),
        rz_catch_rate = mean(rz_catch_rate),
        ez_catch_rate = mean(ez_catch_rate),
        deep_catch_rate = mean(deep_catch_rate),
        short_catch_rate = mean(short_catch_rate),
        receiving_yards_100 = sum(receiving_yards_100),
        receiving_yards_150 = sum(receiving_yards_150),
        receiving_yards_200 = sum(receiving_yards_200),
        receiving_tds_1 = sum(receiving_tds_1),
        receiving_tds_20 = sum(receiving_tds_20),
        receiving_tds_50 = sum(receiving_tds_50),
        
        # special teams
        special_teams_tds = sum(special_teams_tds),
        
        # combo
        touches = sum(touches),
        opportunities = sum(opportunities),
        combo_yards_100 = sum(combo_yards_100),
        combo_yards_150 = sum(combo_yards_150),
        total_yards = sum(total_yards),
        total_tds = sum(total_tds),
        
        # fantasy
        pass_pts = sum(pass_pts),
        rec_pts = sum(rec_pts),
        rush_pts = sum(rush_pts),
        st_pts = sum(st_pts),
        nw_pts = sum(nw_pts),
        fantasy_points = sum(fantasy_points),
        fantasy_points_ppr = sum(fantasy_points_ppr),
      ) %>%
      dplyr::ungroup() %>%
      add_dakota(pbp = pbp, weekly = weekly) %>%
      relocate(dakota, .after = passing_epa) %>%
      relocate(player_id, .before = player_name)
  }
  
  return(player_df)
}
