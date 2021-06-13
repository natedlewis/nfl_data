# nw weighted opportunities ----

# rb opps
wopps_rb <- rb %>% 
  group_by(player_id, season) %>%
  filter((touches/games) > 8) %>% 
  summarise(player_name = last(player_name),
            position = last(position),
            games = games,
            nw_fpts = sum(nw_fpts),
            fpts_per_touch = nw_fpts/touches,
            fpts_per_car = nw_fpts/car,
            fpts_per_five = nw_fpts/in_five_car,
            fpts_per_ten = nw_fpts/in_ten_car,
            fpts_per_neu = nw_fpts/neu_car,
            fpts_per_tgt = nw_fpts/tgts) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  arrange(-fpts_per_touch)

# wr/te opps
wopps_wr <- rec_stats %>% 
  group_by(player_id, season) %>%
  filter((rec/games) > 1.875) %>% 
  summarise(player_name = last(player_name),
            position = last(position),
            games = games,
            nw_fpts = sum(nw_fpts),
            fpts_per_tgt = nw_fpts/tgts,
            fpts_per_rz = nw_fpts/rz_tgts,
            fpts_per_ez = nw_fpts/ez_tgts,
            fpts_per_deep = nw_fpts/deep_tgts) %>% 
            mutate(across(where(is.numeric), round, 2)) %>% 
  arrange(-fpts_per_tgt)

# qb opps
wopps_qb <- qb %>% 
  group_by(player_id, season) %>%
  filter((att/games) > 14) %>% 
  summarise(player_name = last(player_name),
            position = last(position),
            games = games,
            nw_fpts = sum(nw_fpts),
            fpts_per_att = nw_fpts/att,
            fpts_per_rz = nw_fpts/rz_att,
            fpts_per_ez = nw_fpts/ez_att,
            fpts_per_deep = nw_fpts/deep_att,
            fpts_per_car = nw_fpts/car) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  arrange(-fpts_per_att)

# bind rows to single object
wopps <- wopps_qb %>% 
  bind_rows(wopps_rb) %>% 
  bind_rows(wopps_wr)

# trimmed average of nw fpts per variable by position 
avg_wopps <- wopps %>% 
  group_by(position) %>% 
  summarise(fpts_per_tgt = mean(fpts_per_tgt, trim = 0.05, na.rm = TRUE),
            fpts_per_rz = mean(fpts_per_rz, trim = 0.05, na.rm = TRUE),
            fpts_per_ez = mean(fpts_per_ez, trim = 0.05, na.rm = TRUE),
            fpts_per_deep = mean(fpts_per_deep, trim = 0.05, na.rm = TRUE),
            fpts_per_touch = mean(fpts_per_touch, trim = 0.05, na.rm = TRUE),
            fpts_per_car = mean(fpts_per_car, trim = 0.05, na.rm = TRUE),
            fpts_per_five = mean(fpts_per_five, trim = 0.05, na.rm = TRUE),
            fpts_per_ten = mean(fpts_per_ten, trim = 0.05, na.rm = TRUE),
            fpts_per_neu = mean(fpts_per_neu, trim = 0.05, na.rm = TRUE),
            fpts_per_att = mean(fpts_per_att, trim = 0.05, na.rm = TRUE)) %>% 
  mutate(across(where(is.numeric), round, 2),
         type = "nw")

# std weighted opportunities ----

# rb opps
wopps_rb_std <- rb %>% 
  group_by(player_id, season) %>%
  filter((touches/games) > 8) %>% 
  summarise(player_name = last(player_name),
            position = last(position),
            games = games,
            fpts = sum(fpts),
            fpts_per_touch = fpts/touches,
            fpts_per_car = fpts/car,
            fpts_per_five = fpts/in_five_car,
            fpts_per_ten = fpts/in_ten_car,
            fpts_per_neu = fpts/neu_car,
            fpts_per_tgt = fpts/tgts) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  arrange(-fpts_per_touch)

# wr/te opps
wopps_wr_std <- rec %>% 
  group_by(player_id, season) %>%
  filter((rec/games) > 1.875) %>% 
  summarise(player_name = last(player_name),
            position = last(position),
            games = games,
            fpts = sum(fpts),
            fpts_per_tgt = fpts/tgts,
            fpts_per_rz = fpts/rz_tgts,
            fpts_per_ez = fpts/ez_tgts,
            fpts_per_deep = fpts/deep_tgts) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  arrange(-fpts_per_tgt)

# qb opps
wopps_qb_std <- qb %>% 
  group_by(player_id, season) %>%
  filter((att/games) > 14) %>% 
  summarise(player_name = last(player_name),
            position = last(position),
            games = games,
            fpts = sum(fpts),
            fpts_per_att = fpts/att,
            fpts_per_rz = fpts/rz_att,
            fpts_per_ez = fpts/ez_att,
            fpts_per_deep = fpts/deep_att,
            fpts_per_car = fpts/car) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  arrange(-fpts_per_att)

# bind rows to single object
wopps_std <- wopps_qb_std %>% 
  bind_rows(wopps_rb_std) %>% 
  bind_rows(wopps_wr_std)

# trimmed average of fpts per variable by position 

avg_wopps_std <- wopps_std %>% 
  group_by(position) %>% 
  summarise(fpts_per_tgt = mean(fpts_per_tgt, trim = 0.05, na.rm = TRUE),
            fpts_per_rz = mean(fpts_per_rz, trim = 0.05, na.rm = TRUE),
            fpts_per_ez = mean(fpts_per_ez, trim = 0.05, na.rm = TRUE),
            fpts_per_deep = mean(fpts_per_deep, trim = 0.05, na.rm = TRUE),
            fpts_per_touch = mean(fpts_per_touch, trim = 0.05, na.rm = TRUE),
            fpts_per_car = mean(fpts_per_car, trim = 0.05, na.rm = TRUE),
            fpts_per_five = mean(fpts_per_five, trim = 0.05, na.rm = TRUE),
            fpts_per_ten = mean(fpts_per_ten, trim = 0.05, na.rm = TRUE),
            fpts_per_neu = mean(fpts_per_neu, trim = 0.05, na.rm = TRUE),
            fpts_per_att = mean(fpts_per_att, trim = 0.05, na.rm = TRUE)) %>% 
  mutate(across(where(is.numeric), round, 2),
         type = "std")

# join ----

joined <- avg_wopps %>% bind_rows(avg_wopps_std) %>% 
  select(position, type, everything())

# detecting outliers----

upper_bound <- quantile(wopps_qb$fpts_per_car, 0.95, na.rm = TRUE)
lower_bound <- quantile(wopps_qb$fpts_per_car, 0.05, na.rm = TRUE)

outlier_ind <- which(wopps_qb$fpts_per_car < lower_bound | wopps_qb$fpts_per_car > upper_bound)

wopps_qb[outlier_ind, "fpts_per_car"]