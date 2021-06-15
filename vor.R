season <- read_csv("season_stats.csv")

season <- season %>% group_by(season) %>% arrange(overall) %>% mutate(adp_rk = 1:n()) %>% ungroup()

baseline_qb <- season %>% filter(nw_position == "QB", nw_pos_rk == 10) %>% select(season, nw_position, nw_fpts, pick)

baseline_rb <- season %>% filter(nw_position == "RB", nw_pos_rk == 30) %>% select(season, nw_position, nw_fpts, pick)

baseline_rec <-season %>% filter(nw_position == "WR/TE", nw_pos_rk ==30) %>% select(season, nw_position, nw_fpts, pick)

baseline_all <- baseline_qb %>% bind_rows(baseline_rb) %>% bind_rows(baseline_rec) %>% rename(baseline = nw_fpts)

baseline_avg <- baseline_all %>% group_by(nw_position) %>% summarise(avg_baseline = mean(baseline, na.rm = TRUE))

season <- season %>% left_join(baseline_all, by = c("season", "nw_position"))

season <- season %>% mutate(vor = nw_fpts - baseline)

vor <- season %>% select(player_id:nw_position, adp_rk, nw_fpts, baseline, vor) %>% arrange(-season, adp_rk)



ggplot(vor, aes(x = vor, y = nw_position, fill = nw_position)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")


x <- vor %>% group_by(adp_rk) %>% 
  summarise(games = sum(games),
            avg_vor = mean(vor, trim = 0.05, na.rm = TRUE)) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  arrange(adp_rk)
         

pick_values <- vor %>% select(adp_rk, vor) %>% arrange(adp_rk)

# Library
library(ggplot2)
library(hrbrthemes)

# Create dummy data
data <- pick_values(
  cond = rep(c("condition_1", "condition_2"), each=10), 
  my_x =  + rnorm(100,sd=9), 
  my_y = 1:100 + rnorm(100,sd=16) 
)

# linear trend + confidence interval
p3 <- ggplot(as.data.frame(pick_values), aes(x=pick, y=vor)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

# Basic scatter plot.
p3 <- ggplot(pick_values, aes(x=overall, y=vor)) +
  geom_point( color="#69b3a2") +
  theme_ipsum()

ggplot(x, aes(x=adp_rk, y=vor)) + 
  geom_point()

  facet_wrap(~nw_position, scale="free_y")

