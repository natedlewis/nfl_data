# Load raw roster data
raw_rosters <- fast_scraper_roster(2010:2021)

# Load dynastyprocess player ids
raw_dp_rosters <-
  read_csv(
    "https://raw.githubusercontent.com/dynastyprocess/data/master/files/db_playerids.csv"
  ) %>%
  select(
    ends_with("_id", ignore.case = TRUE, vars = NULL),
    draft_year,
    draft_round,
    draft_pick
  ) %>%
  mutate(sleeper_id = as.character(sleeper_id))

# Join roster data
raw_rosters <- raw_rosters %>%
  left_join(raw_dp_rosters)

cleaned_rosters <- raw_rosters %>%
  filter(!is.na(position)) %>%
  mutate(
    nw_position = case_when(
      position == "TE" | position == "WR" ~ "WR/TE",
      position == "FB" ~ "RB",
      TRUE ~ position
    ),
    side_of_ball = ifelse(position %in% c("QB", "RB", "WR", "TE", "FB"), "OFF", "DEF")
  ) %>%
  select(
    gsis_id,
    season,
    full_name,
    position,
    depth_chart_position,
    nw_position,
    side_of_ball,
    years_exp,
    birth_date,
    height,
    weight,
    college,
    draft_year,
    draft_round,
    draft_pick,
    status,
    ends_with("_id", vars = NULL),
    everything()
  ) %>%
  arrange(-season, gsis_id)

# offense only
rosters_offense <- cleaned_rosters %>% filter(side_of_ball == "OFF")

# export to data folder
write_csv(cleaned_rosters, "./data/rosters.csv")
