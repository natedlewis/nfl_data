library(tidyverse)
library(glue)

# load scrape func
scrape_page <- 
  function(page){
    read_csv(glue("https://raw.githubusercontent.com/jchernak96/NFL-Injury-Data-PFR-/master/Data/PFR_{page}_Injuries.csv"))
  }

# scrape raw injury data from pfr
raw_injuries <- 
  tibble(year = 2012:2020) %>% 
  mutate(year_df = map(year, scrape_page)) %>% 
  unnest(year_df)

# clean df
injuries_cl <- raw_injuries %>%
  relocate(gsis_id, .before = X1) %>%
  filter(!is.na(gsis_id) & position %in% c("WR", "TE", "RB", "QB", "FB")) %>% 
  rename(player_id = gsis_id,
         season = Season,
         week = Week,
         full_name = Name,
         age = Age_Start_Season) %>% 
  arrange(season, week, team_abbr, full_name)

names(injuries_cl) <- tolower(names(injuries_cl))
names(injuries_cl) <- gsub(x = names(injuries_cl), pattern = "\\.", replacement = "_")

# only distinct
inj_wkly <- subset(injuries_cl, !duplicated(subset(injuries_cl, select=c(player_id, season, week)))) %>% 
  select(player_id, season, week, active_inactive:special_teams_snap_rate, age, -position)
