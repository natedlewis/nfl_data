# Weighted touches ----
weighted_opps_rbs_avg <-
  overall %>%
  filter(
    position == "RB",
    touches / games > 8
  ) %>%
  group_by(season) %>%
  mutate(
    rz_carries = rz_carries - inside_ten_carries,
    inside_ten_carries = inside_ten_carries - inside_five_carries,
    outside_rz_targets = outside_rz_targets - deep_targets
  ) %>%
  summarise(
    qual_rbs = n(),
    total_rush_pts = sum(rush_pts),
    total_carries = sum(carries),
    total_outside_rz_carries = sum(outside_rz_carries),
    total_rz_carries = sum(rz_carries),
    total_inside_ten_carries = sum(inside_ten_carries),
    total_inside_five_carries = sum(inside_five_carries),
    total_neutral_carries = sum(neutral_carries),
    total_garbadge_time_carries = sum(garbadge_time_carries),
    total_rec_pts = sum(rec_pts),
    total_targets = sum(targets),
    total_outside_rz_targets = sum(outside_rz_targets),
    total_rz_targets = sum(rz_targets),
    total_ez_targets = sum(ez_targets),
    total_deep_targets = sum(deep_targets),
    total_short_targets = sum(short_targets),
    per_carry = total_rush_pts / total_carries,
    per_outside_rz_carry = total_rush_pts / total_outside_rz_carries,
    per_rz_carry = total_rush_pts / total_rz_carries,
    per_inside_ten_carry = total_rush_pts / total_inside_ten_carries,
    per_inside_five_carry = total_rush_pts / total_inside_five_carries,
    per_neutral_carry = total_rush_pts / total_neutral_carries,
    per_garbadge_time_carry = total_rush_pts / total_garbadge_time_carries,
    per_target = total_rec_pts / total_targets,
    per_outside_rz_target = total_rec_pts / total_outside_rz_targets,
    per_rz_target = total_rec_pts / total_rz_targets,
    per_ez_target = total_rec_pts / total_ez_targets,
    per_deep_target = total_rec_pts / total_deep_targets,
    per_short_target = total_rec_pts / total_short_targets
  ) %>%
  ungroup() %>%
  summarise(
    avg_per_carry = mean(per_carry, trim = 0.05),
    avg_per_outside_rz_carry = mean(per_outside_rz_carry, trim = 0.05),
    avg_per_rz_carry = mean(per_rz_carry, trim = 0.05),
    avg_per_inside_ten_carry = mean(per_inside_ten_carry, trim = 0.05),
    avg_per_inside_five_carry = mean(per_inside_five_carry, trim = 0.05),
    avg_per_neutral_carry = mean(per_neutral_carry, trim = 0.05),
    avg_per_garbadge_time_carry = mean(per_garbadge_time_carry, trim = 0.05),
    avg_per_target = mean(per_target, trim = 0.05),
    avg_per_outside_rz_target = mean(per_outside_rz_target, trim = 0.05),
    avg_per_rz_target = mean(per_rz_target, trim = 0.05),
    avg_per_ez_target = mean(per_ez_target, trim = 0.05),
    avg_per_deep_target = mean(per_deep_target, trim = 0.05),
    avg_per_short_target = mean(per_short_target, trim = 0.05)
  )

weighted_opps_rbs <- overall %>%
  filter(position == "RB") %>%
  group_by(player_id, season) %>%
  mutate(
    rz_carries_real = rz_carries - inside_ten_carries,
    inside_ten_carries_real = inside_ten_carries - inside_five_carries,
    outside_rz_targets_real = outside_rz_targets - deep_targets,
    opps = sum(
      outside_rz_carries * weighted_opps_rbs_avg$avg_per_outside_rz_carry,
      rz_carries_real * weighted_opps_rbs_avg$avg_per_rz_carry,
      inside_ten_carries_real * weighted_opps_rbs_avg$avg_per_inside_ten_carry,
      inside_five_carries * weighted_opps_rbs_avg$avg_per_inside_five_carry,
      outside_rz_targets_real * weighted_opps_rbs_avg$avg_per_outside_rz_target,
      rz_targets * weighted_opps_rbs_avg$avg_per_rz_target,
      deep_targets * weighted_opps_rbs_avg$avg_per_deep_target
    )
  ) %>%
  group_by(season) %>%
  arrange(-opps) %>%
  mutate(
    opps_rk = 1:n(),
    efficiency = opps_rk - nw_rk
  ) %>%
  ungroup() %>%
  select(player_id, season, opps, opps_rk, efficiency)

# Weighted targets ----
weighted_opps_receivers_avg <-
  overall %>% filter(receptions / games >= 1.875, position %in% c("WR", "TE")) %>% 
  group_by(season) %>% 
  summarise(
    qual_receivers = n(),
    total_rec_pts = sum(rec_pts),
    total_targets = sum(targets),
    total_outside_rz_targets = sum(outside_rz_targets),
    total_rz_targets = sum(rz_targets),
    total_ez_targets = sum(ez_targets),
    total_ez_targets_inside_rz = sum(ez_targets_inside_rz),
    total_ez_targets_outside_rz = sum(ez_targets_outside_rz),
    total_deep_targets = sum(deep_targets),
    total_short_targets = sum(short_targets),
    total_neutral_targets = sum(neutral_targets),
    total_garbadge_time_targets = sum(garbadge_time_targets),
    per_target = total_rec_pts / total_targets,
    per_outside_rz_target = total_rec_pts / total_outside_rz_targets,
    per_rz_target = total_rec_pts / total_rz_targets,
    per_ez_target = total_rec_pts / total_ez_targets,
    per_ez_target_inside_rz = total_rec_pts / total_ez_targets_inside_rz,
    per_ez_target_outside_rz = total_rec_pts / total_ez_targets_outside_rz,
    per_deep_target = total_rec_pts / total_deep_targets,
    per_short_target = total_rec_pts / total_short_targets,
    per_neutral_target = total_rec_pts / total_neutral_targets,
    per_garbadge_time_target = total_rec_pts / total_garbadge_time_targets
  ) %>%
  ungroup() %>% 
  summarise(
    avg_per_target = mean(per_target, trim = 0.05),
    avg_per_outside_rz_target = mean(per_outside_rz_target, trim = 0.05),
    avg_per_rz_target = mean(per_rz_target, trim = 0.05),
    avg_per_ez_target = mean(per_ez_target, trim = 0.05),
    avg_per_ez_target_inside_rz = mean(per_ez_target_inside_rz, trim = 0.05),
    avg_per_ez_target_outside_rz = mean(per_ez_target_outside_rz, trim = 0.05),
    avg_per_deep_target = mean(per_deep_target, trim = 0.05),
    avg_per_short_target = mean(per_short_target, trim = 0.05),
    avg_per_neutral_target = mean(per_neutral_target, trim = 0.05),
    avg_per_garbadge_time_target = mean(per_garbadge_time_target, trim = 0.05)
  )

weighted_opps_receivers <- overall %>%
  filter(position %in% c("WR", "TE")) %>%
  group_by(player_id, season) %>%
  mutate(
    opps = sum(
      outside_rz_targets * weighted_opps_receivers_avg$avg_per_outside_rz_target,
      rz_targets * weighted_opps_receivers_avg$avg_per_rz_target,
      ez_targets_inside_rz * weighted_opps_receivers_avg$avg_per_ez_target_inside_rz,
      ez_targets_outside_rz * weighted_opps_receivers_avg$avg_per_ez_target_outside_rz,
      deep_targets * weighted_opps_receivers_avg$avg_per_deep_target,
      short_targets * weighted_opps_receivers_avg$avg_per_short_target
    )
  ) %>%
  group_by(season) %>%
  arrange(-opps) %>%
  mutate(
    opps_rk = 1:n(),
    efficiency = opps_rk - nw_rk
  ) %>%
  ungroup() %>%
  select(player_id, season, opps, opps_rk, efficiency)

# Weighted attempts ----
weighted_opps_qbs_avg <-
  overall %>% filter(position == "QB", attempts / games > 14) %>% group_by(season) %>% 
  summarise(
    qual_qbs = n(),
    total_pass_pts = sum(pass_pts),
    total_attempts = sum(attempts),
    total_outside_rz_attempts= sum(outside_rz_attempts),
    total_rz_attempts = sum(rz_attempts),
    total_ez_attempts = sum(ez_attempts),
    total_ez_attempts_inside_rz = sum(ez_attempts_inside_rz),
    total_ez_attempts_outside_rz = sum(ez_attempts_outside_rz),
    total_deep_attempts = sum(deep_attempts),
    total_short_attempts = sum(short_attempts),
    total_neutral_attempts = sum(neutral_attempts),
    total_garbadge_time_attempts = sum(garbadge_time_attempts),
    total_rush_pts = sum(rush_pts),
    total_carries = sum(carries),
    total_rz_carries = sum(rz_carries),
    per_attempt = total_pass_pts / total_attempts,
    per_outside_rz_attempt = total_pass_pts / total_outside_rz_attempts,
    per_rz_attempt = total_pass_pts / total_rz_attempts,
    per_ez_attempt = total_pass_pts / total_ez_attempts,
    per_ez_attempt_inside_rz = total_pass_pts / total_ez_attempts_inside_rz,
    per_ez_attempt_outside_rz = total_pass_pts / total_ez_attempts_outside_rz,
    per_deep_attempt = total_pass_pts / total_deep_attempts,
    per_short_attempt = total_pass_pts / total_short_attempts,
    per_neutral_attempt = total_pass_pts / total_neutral_attempts,
    per_garbadge_time_attempt = total_pass_pts / total_garbadge_time_attempts,
    per_carry = total_rush_pts / total_carries,
    per_rz_carry = total_rush_pts / total_rz_carries
  ) %>%
  ungroup() %>%
  summarise(
    avg_per_attempt = mean(per_attempt, trim = 0.2),
    avg_per_outside_rz_attempt = mean(per_outside_rz_attempt, trim = 0.2),
    avg_per_rz_attempt = mean(per_rz_attempt, trim = 0.2),
    avg_per_ez_attempt = mean(per_ez_attempt, trim = 0.2),
    avg_per_ez_attempt_inside_rz = mean(per_ez_attempt_inside_rz, trim = 0.2),
    avg_per_ez_attempt_outside_rz = mean(per_ez_attempt_outside_rz, trim = 0.2),
    avg_per_deep_attempt = mean(per_deep_attempt, trim = 0.2),
    avg_per_short_attempt = mean(per_short_attempt, trim = 0.2),
    avg_per_neutral_attempt = mean(per_neutral_attempt, trim = 0.2),
    avg_per_garbadge_time_attempt = mean(per_garbadge_time_attempt, trim = 0.2),
    avg_per_carry = mean(per_carry, trim = 0.2),
    avg_per_rz_carry = mean(per_rz_carry, trim = 0.2)
  )

weighted_opps_qbs<- overall %>%
  filter(position == "QB") %>%
  group_by(player_id, season) %>%
  mutate(
    opps = sum(
      outside_rz_attempts * weighted_opps_qbs_avg$avg_per_outside_rz_attempt,
      rz_attempts * weighted_opps_qbs_avg$avg_per_rz_attempt,
      ez_attempts_inside_rz * weighted_opps_qbs_avg$avg_per_ez_attempt_inside_rz,
      ez_attempts_outside_rz * weighted_opps_qbs_avg$avg_per_ez_attempt_outside_rz,
      deep_attempts * weighted_opps_qbs_avg$avg_per_deep_attempt,
      short_attempts * weighted_opps_qbs_avg$avg_per_short_attempt,
      neutral_attempts * weighted_opps_qbs_avg$avg_per_neutral_attempt,
      carries * weighted_opps_qbs_avg$avg_per_carry,
      rz_carries * weighted_opps_qbs_avg$avg_per_rz_carry
    )
  ) %>%
  group_by(season) %>%
  arrange(-opps) %>%
  mutate(
    opps_rk = 1:n(),
    efficiency = opps_rk - nw_rk
  ) %>%
  ungroup() %>%
  select(player_id, season, opps, opps_rk, efficiency)
