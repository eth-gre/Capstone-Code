# === Load libraries
library(AER)
library(stargazer)
library(dplyr)

df_individual_education = read.csv("data/upd4_hh_c.csv")
df_individual_education <- df_individual_education %>% select(UPI, round, r_hhid, r_id, grade_label = hc_07)

educ_years_map <- c(
  "PP" = 0,
  "ADULT" = NA,
  
  "D1" = 1, "D2" = 2, "D3" = 3, "D4" = 4, "D5" = 5, "D6" = 6, "D7" = 7, "D8" = 8,
  "F1" = 9, "F2" = 10, "F3" = 11, "F4" = 12, "F5" = 13, "F6" = 14,
  "U1" = 13, "U2" = 14, "U3" = 15, "U4" = 16, "U5&+" = 17,
  
  "OSC" = 13,
  "MS+ COURSE" = 13,
  "'O'+ COURSE" = 13,
  "'A'+ COURSE" = 13,
  "DIPLOMA" = 13
)

df_individual_education$years_educ <- educ_years_map[df_individual_education$grade_label]



# === Derive progression flags: did round-1 primary/high-schoolers reach higher levels in later rounds?
df_education_progress <- df_individual_education %>%
  filter(!is.na(years_educ)) %>%
  group_by(UPI) %>%
  mutate(round1_educ = years_educ[round == 1][1]) %>%   # NA if not observed in round 1
  filter(!is.na(round1_educ)) %>%
  summarise(
    round1_educ = first(round1_educ),
    max_future_educ = suppressWarnings(max(years_educ[round > 1], na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(max_future_educ = ifelse(is.infinite(max_future_educ), NA, max_future_educ)) %>%
  mutate(
    # Only defined for those who started in PRIMARY (years_educ < 9) in round 1
    primary_school_will_attend_high_school = ifelse(
      round1_educ < 9,
      as.integer(!is.na(max_future_educ) & max_future_educ >= 9),
      NA
    ),
    primary_school_will_attend_higher_ed = ifelse(
      round1_educ < 9,
      as.integer(!is.na(max_future_educ) & max_future_educ >= 13),
      NA
    ),
    # Only defined for those who started in HIGH SCHOOL (9 <= years_educ < 13) in round 1
    high_school_will_attend_higher_ed = ifelse(
      round1_educ >= 9 & round1_educ < 13,
      as.integer(!is.na(max_future_educ) & max_future_educ >= 13),
      NA
    )
  ) %>%
  select(UPI, primary_school_will_attend_high_school, 
         primary_school_will_attend_higher_ed, 
         high_school_will_attend_higher_ed)


table(df_individual_education$round)

# 2. How many people have ANY round > 1 observation at all?
df_individual_education %>%
  group_by(UPI) %>%
  summarise(n_rounds = n_distinct(round), has_future = any(round > 1)) %>%
  count(has_future)

# 3. Direct trace of one specific bracket to see where it's failing
debug_check <- df_individual_education %>%
  filter(!is.na(years_educ)) %>%
  group_by(UPI) %>%
  mutate(round1_educ = years_educ[round == 1][1]) %>%
  filter(!is.na(round1_educ)) %>%
  summarise(
    round1_educ = first(round1_educ),
    n_future_obs = sum(round > 1),                 # <- how many future obs exist
    max_future_educ_raw = suppressWarnings(max(years_educ[round > 1], na.rm = TRUE)),
    .groups = "drop"
  )

# How many people in the primary bracket actually HAVE future observations?
debug_check %>% 
  filter(round1_educ < 9) %>% 
  summarise(
    total = n(),
    has_future_obs = sum(n_future_obs > 0),
    is_neg_inf = sum(is.infinite(max_future_educ_raw))
  )