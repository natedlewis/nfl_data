library(glue)

# helper functions
scrape_page <- function(page) {
  read_csv(
    glue(
      "https://raw.githubusercontent.com/jchernak96/NFL-Injury-Data-PFR-/master/Data/PFR_{page}_Injuries.csv"
    )
  )
}

# clean injury/snap data ----
raw_injuries <-
  tibble(year = 2010:2020) %>%
  mutate(year_df = map(year, scrape_page)) %>%
  unnest(year_df)

names(raw_injuries) <- tolower(names(raw_injuries))
names(raw_injuries) <-
  gsub(
    x = names(raw_injuries),
    pattern = "\\.",
    replacement = "_"
  )

# distinct
injuries_cleaned <-
  subset(raw_injuries, !duplicated(subset(
    raw_injuries,
    select = c(gsis_id, season, week)
  ))) %>%
  select(
    gsis_id,
    season,
    week,
    team_abbr,
    active_inactive,
    game_designation,
    injury_type,
    started:special_teams_snap_rate
  ) %>%
  filter(!is.na(gsis_id)) %>%
  rename(player_id = gsis_id)

# add binary indicators for snaps
injuries_cleaned <- injuries_cleaned %>%
  group_by(player_id, season, week) %>%
  mutate(
    started_bi = ifelse(started == "YES", 1, 0),
    active_bi = ifelse(active_inactive == "Active", 1, 0),
    healthy_bi = ifelse(game_designation == "Healthy", 1, 0),
    over_half_bi = ifelse(offense_snap_rate >= .5, 1, 0)
  ) %>%
  rename(recent_team = team_abbr) %>% 
  ungroup()

# export to data folder
write_csv(injuries_cleaned, "./data/snaps.csv")
