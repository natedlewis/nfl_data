library(reactable)

rb_tbl <-
  season %>%
  filter(position == "RB") %>%
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
      
      # rushing
      "carries",
      "rushing_yards",
      "rushing_tds",
      "rushing_epa",
      "outside_rz_carries",
      "rz_carries",
      "inside_ten_carries",
      "inside_five_carries",
      "neutral_carries",
      "garbadge_time_carries",
      
      # combo
      "touches",
      "touches_per_game",
      "receptions",
      "targets",
      "outside_rz_targets",
      "rz_targets",
      "deep_targets",
      "total_yards",
      "total_tds",
      
      # snaps
      "offense_snaps",
      "offense_snap_rate",
      
      # fantasy
      "nw_pts",
      "nw_pts_per_game",
      "nw_pts_per_snap",
      
      # ids
      "player_id"
    )
  )) %>% left_join(shares %>% select(player_id, season, touch_share, carry_share, inside_ten_carry_share), by = c("player_id", "season")) %>% 
  left_join(nw_pts_var %>% select(player_id, season, nw_pts_cv), by = c("player_id", "season")) %>% 
  group_by(player_id, season) %>%
  mutate(
    rz_carries = rz_carries - inside_ten_carries,
    inside_ten_carries = inside_ten_carries - inside_five_carries,
    outside_rz_targets = outside_rz_targets - deep_targets,
    opps = sum(
      outside_rz_carries * rb_pts_per_touch$avg_per_outside_rz_carry,
      rz_carries * rb_pts_per_touch$avg_per_rz_carry,
      inside_ten_carries * rb_pts_per_touch$avg_per_inside_ten_carry,
      inside_five_carries * rb_pts_per_touch$avg_per_inside_five_carry,
      outside_rz_targets * rb_pts_per_touch$avg_per_outside_rz_target,
      rz_targets * rb_pts_per_touch$avg_per_rz_target,
      deep_targets * rb_pts_per_touch$avg_per_deep_target
    )
  ) %>% 
  ungroup() %>% 
  arrange(-opps) %>% 
  dplyr::group_by(season) %>%
  dplyr::mutate(opps_rk = 1:n(),
                efficiency = opps_rk - nw_rk) %>%
  ungroup() %>% 
  arrange(-season, nw_rk) %>% 
  select(-player_id, -deep_targets, -outside_rz_targets)

reactable(
  rb_tbl,
  columns = list(
    nw_rk = colDef(name = "Rk"),
    season = colDef(name = "Season"),
    full_name = colDef(name = "Name",
                       width = 160),
    recent_team = colDef(name = "Team",
                         width = 55),
    position = colDef(name = "Pos",
                         width = 55),
    games = colDef(name = "G"),
    years_exp = colDef(name = "Exp"),
    carries = colDef(name = "Total"),
    rushing_yards = colDef(name = "Yds",
                           format = colFormat(separators = TRUE)),
    rushing_tds = colDef(name = "TDs"),
    touches = colDef(name = "Touches"),
    outside_rz_carries = colDef(name = "Outside RZ"),
    rz_carries = colDef(name = "RZ"),
    inside_ten_carries = colDef(name = "Inside 10"),
    inside_five_carries = colDef(name = "Inside 5"),
    neutral_carries = colDef(name = "Neutral"),
    garbadge_time_carries = colDef(name = "Garbadge"),
    receptions = colDef(name = "Rec"),
    targets = colDef(name = "Tgts"),
    rz_targets = colDef(name = "RZ"),
    offense_snaps = colDef(name = "Snaps"),
    offense_snap_rate = colDef(name = "Snap %",
                               format = colFormat(percent = TRUE, digits = 0)),
    opps = colDef(name = "Opps",
                  format = colFormat(digits = 0)),
    opps_rk = colDef(name = "Opps Rk"),
    touch_share = colDef(name = "Touch %",
                         format = colFormat(percent = TRUE, digits = 0)),
    carry_share = colDef(name = "Carry %",
                         format = colFormat(percent = TRUE, digits = 0)),
    inside_ten_carry_share = colDef(name = "Inside 10 %",
                                    format = colFormat(percent = TRUE, digits = 0)),
    touches_per_game = colDef(name = "Touches/G"),
    nw_pts = colDef(name = "Pts"),
    nw_pts_per_game = colDef(name = "Pts/G"),
    nw_pts_per_snap = colDef(name = "Pts/Snap"),
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
  columnGroups = list(
    colGroup(
      columns = c(
        "carries",
        "rushing_yards",
        "rushing_tds"
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
      columns = c(
        "touches",
        "touch_share",
        "opps",
        "opps_rk",
        "offense_snaps",
        "offense_snap_rate"
      )
    ),
    colGroup(
      name = "Carries",
      columns = c(
        "outside_rz_carries",
        "rz_carries",
        "carry_share",
        "inside_ten_carries",
        "inside_ten_carry_share",
        "inside_five_carries",
        "neutral_carries",
        "garbadge_time_carries"
      )
    ),
    colGroup(name = "Receiving", columns = c(
      "receptions",
      "targets", 
      "rz_targets"))
  ),
  theme = reactableTheme(
    borderColor = "#dfe2e5",
    stripedColor = "#f6f8fa",
    highlightColor = "#f0f5f9",
    cellPadding = "8px 12px",
    style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
    searchInputStyle = list(width = "100%")
  )
)
