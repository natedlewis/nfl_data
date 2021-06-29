# create data frame ----
rec_tbl <-
  season %>%
  filter(position == "WR" | position == "TE") %>%
  dplyr::select(tidyselect::any_of(
    c(
      # basic info
      "nw_rk",
      "season",
      "full_name",
      "recent_team",
      "position",
      "games",
      "years_exp",
      
      # receiving
      "receptions",
      "targets",
      "receiving_yards",
      "receiving_tds",
      "receiving_air_yards",
      "receiving_yards_after_catch",
      "outside_rz_targets",
      "rz_targets",
      "ez_targets",
      "ez_targets_inside_rz",
      "ez_targets_outside_rz",
      "deep_targets",
      "short_targets",
      "adot",
      "catch_rate",
      "ez_catch_rate",
      "deep_catch_rate",
      "rec_yds_100",
      "rec_yds_150",
      "rec_yds_200",
      "offense_snaps",
      "offense_snap_rate",
      "targets_per_game",
      
      # fantasy
      "nw_pts",
      "nw_pts_per_game",
      "nw_pts_per_snap",
      
      # ids
      "player_id"
    )
  )) %>% left_join(shares %>% select(player_id, season, target_share, rz_target_share, ez_target_share), by = c("player_id", "season")) %>% 
  left_join(nw_pts_var %>% select(player_id, season, nw_pts_cv), by = c("player_id", "season")) %>% 
  group_by(player_id, season) %>%
  mutate(
    opps = sum(
      targets * rec_pts_per_target$avg_per_target,
      outside_rz_targets * rec_pts_per_target$avg_per_outside_rz_target,
      rz_targets * rec_pts_per_target$avg_per_rz_target,
      ez_targets_inside_rz * rec_pts_per_target$avg_per_ez_target_inside_rz,
      ez_targets_outside_rz * rec_pts_per_target$avg_per_ez_target_outside_rz,
      deep_targets * rec_pts_per_target$avg_per_deep_target,
      short_targets * rec_pts_per_target$avg_per_short_target
    )
  ) %>% 
  ungroup() %>% 
  arrange(-opps) %>% 
  dplyr::group_by(season) %>%
  dplyr::mutate(opps_rk = 1:n(),
                efficiency = opps_rk - nw_rk) %>%
  ungroup() %>% 
  arrange(-season, nw_rk) %>% 
  relocate(player_id, .after = efficiency) %>% 
  select(-outside_rz_targets, -ez_targets_inside_rz, -ez_targets_outside_rz, -short_targets)

# create table ----
reactable(
  rec_tbl %>% filter(nw_rk <= 56),
  columns = list(
    nw_rk = colDef(name = "RK"),
    season = colDef(name = "SEASON"),
    full_name = colDef(name = "NAME",
                       width = 160),
    recent_team = colDef(name = "TM",
                         width = 55),
    position = colDef(name = "POS",
                      width = 55),
    games = colDef(name = "G"),
    years_exp = colDef(name = "EXP"),
    receptions = colDef(name = "REC"),
    targets = colDef(name = "TGTS"),
    receiving_yards = colDef(name = "YDS",
                             format = colFormat(separators = TRUE)),
    receiving_tds = colDef(name = "TDS"),
    receiving_air_yards = colDef(name = "AIR",
                                 format = colFormat(separators = TRUE)),
    receiving_yards_after_catch = colDef(name = "YAC",
                                         format = colFormat(separators = TRUE)),
    rz_targets = colDef(name = "RZ"),
    ez_targets = colDef(name = "EZ"),
    deep_targets = colDef(name = "DEEP"),
    adot = colDef(name = "ADOT",
                  format = colFormat(digits = 1)),
    catch_rate = colDef(name = "CATCH RT",
                        format = colFormat(percent = TRUE, digits = 0)),
    ez_catch_rate = colDef(name = "EZ CATCH RT",
                           format = colFormat(percent = TRUE, digits = 0)),
    deep_catch_rate = colDef(name = "DEEP CATCH RT",
                             format = colFormat(percent = TRUE, digits = 0)),
    rec_yds_100 = colDef(name = "100"),
    rec_yds_150 = colDef(name = "150"),
    rec_yds_200 = colDef(name = "200"),
    offense_snaps = colDef(name = "SNAPS"),
    offense_snap_rate = colDef(name = "SNAP %",
                               format = colFormat(percent = TRUE, digits = 0)),
    opps = colDef(name = "OPPS",
                  format = colFormat(digits = 0)),
    opps_rk = colDef(name = "OPPS RK"),
    target_share = colDef(name = "TGT %",
                          format = colFormat(percent = TRUE, digits = 0)),
    rz_target_share = colDef(name = "RZ %",
                             format = colFormat(percent = TRUE, digits = 0)),
    ez_target_share = colDef(name = "EZ %",
                             format = colFormat(percent = TRUE, digits = 0)),
    targets_per_game = colDef(name = "TGTS/G",
                              format = colFormat(digits = 1)),
    nw_pts = colDef(name = "PTS"),
    nw_pts_per_game = colDef(name = "PTS/G",
                             format = colFormat(digits = 1)),
    nw_pts_per_snap = colDef(name = "PTS/S"),
    nw_pts_cv = colDef(name = "CV",
                       format = colFormat(percent = FALSE, digits = 2)),
    efficiency = colDef(
      name = "Efficiency",
      style = function(diff) {
        if (diff > 0) {
          color <- "#008000"
        } else if (diff < 0) {
          color <- "#e00000"
        } else {
          color <- "#777"
        }
        list(color = color, fontWeight = "bold")
      }
    )
  ),
  filter = TRUE,
  striped = TRUE,
  compact = TRUE,
  highlight = TRUE,
  wrap = FALSE,
  minRows = 10,
  paginationType = "simple",
  defaultPageSize = 30,
    theme = reactableTheme(
      borderColor = "#dfe2e5",
      stripedColor = "#f6f8fa",
      highlightColor = "#f0f5f9",
      cellPadding = "8px 12px",
      style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
      searchInputStyle = list(width = "100%")
    )
  )


columnGroups = list(
  colGroup(
    columns = c(
      "receptions",
      "targets",
      "receiving_yards",
      "receiving_tds",
      "receiving_air_yards",
      "receiving_yards_aftch_catch"
    )
  ),
  colGroup(
    name = "Performance",
    columns = c(
      "nw_pts",
      "nw_pts_cv",
      "nw_pts_per_game",
      "nw_pts_per_snap",
      "efficiency"
    )
  ),
  colGroup(
    name = "Opportunity",
    columns = c("opps",
                "opps_rk",
                "offense_snaps",
                "offense_snap_rate")
  ),
  colGroup(
    name = "Targets",
    columns = c(
      "targets_per_game",
      "target_share",
      "rz_targets",
      "rz_target_share",
      "ez_targets",
      "ez_catch_rate",
      "ez_target_share",
      "deep_targets",
      "deep_catch_rate"
    )
  ),  