# load/clean roster
raw_roster <- fast_scraper_roster(2010:2021) %>% 
  progressr::with_progress()
roster_cl <- raw_roster %>% filter(!is.na(gsis_id))

# load player ids
raw_ids <- ffscrapr::dp_playerids()
ids_cl <- raw_ids %>% filter(!is.na(gsis_id)) %>% 
  select(gsis_id, draft_year, draft_round, draft_pick, fantasypros_id, pfr_id, mfl_id, merge_name)

#join ids to roster and tidy select
roster <- roster_cl %>% left_join(ids_cl, by = c("gsis_id")) %>% 
  rename(player_id = gsis_id) 

rookies <- raw_roster %>% 
  filter(season == 2021, years_exp == 0, position %in% c("WR", "TE", "RB", "QB")) %>% 
  left_join(raw_ids %>% filter(!is.na(sleeper_id), draft_year == 2021) %>% 
              select(sleeper_id, draft_year, draft_round, draft_pick, fantasypros_id, pfr_id, mfl_id, merge_name), 
            by = c("sleeper_id")) %>% 
  arrange(draft_round, draft_pick)

roster <- bind_rows(roster, rookies) %>% 
  dplyr::select(tidyselect::any_of(c(
    
    # basic information
    "player_id", "season", "full_name", "team", "position", "years_exp", "height", "weight",
    
    # draft info
    "college", "draft_year", "draft_round", "draft_pick", "depth_chart_position",
    
    # other ids
    "pff_id", "fantasy_data_id", "pfr_id", "fantasypros_id", "mfl_id", "sleeper_id"))) %>% 
  arrange(-season, team, position) %>% 
  filter(position %in% c("WR", "TE", "RB", "QB"))

pj_roster <- roster %>% select(-team)


