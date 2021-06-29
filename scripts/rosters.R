# Load raw roster data
raw_rosters <- fast_scraper_roster(2010:2021)

# Load dynastyprocess player ids
raw_dp_rosters <-
  read_csv(
    "https://raw.githubusercontent.com/dynastyprocess/data/master/files/db_playerids.csv"
  ) %>%
  select(ends_with("_id", ignore.case = TRUE, vars = NULL),
         draft_year,
         draft_round,
         draft_pick) %>%
  mutate(sleeper_id = as.character(sleeper_id))

# Join roster data
rosters <- raw_rosters %>%
  left_join(raw_dp_rosters) %>%
  filter(!is.na(position)) %>%
  select(
    season,
    full_name,
    birth_date,
    height,
    weight,
    college,
    draft_year,
    draft_round,
    draft_pick,
    status,
    ends_with("_id", vars = NULL)
  )

# Export
write_csv(rosters, "rosters.csv")