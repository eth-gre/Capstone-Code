# === Load libraries
library(AER)
library(stargazer)
library(dplyr)

TARGET_ROUND <- 1

df_personal_information = read.csv("data/upd4_hh_b.csv")
df_personal_information <- df_personal_information %>% select(UPI, round, r_hhid, r_id, sex_label = hb_02, age = hb_04, marriage_label = hb_19)

df_individual_education = read.csv("data/upd4_hh_c.csv")
df_individual_education <- df_individual_education %>% select(UPI, round, r_hhid, r_id, grade_label = hc_07, educ_cost = hc_28_8)

educ_years_map <- c(
  "PP" = 0,
  "ADULT" = NA,
  
  "D1" = 1, "D2" = 2, "D3" = 3, "D4" = 4, "D5" = 5, "D6" = 6, "D7" = 7, "D8" = 8,
  "F1" = 9, "F2" = 10, "F3" = 11, "F4" = 12, "F5" = 13, "F6" = 14,
  "U1" = 13, "U2" = 14, "U3" = 15, "U4" = 16, "U5&+" = 17,
  
  "OSC" = NA,
  "MS+ COURSE" = NA,
  "'O'+ COURSE" = NA,
  "'A'+ COURSE" = NA,
  "DIPLOMA" = NA
)

df_individual_education$years_educ <- educ_years_map[df_individual_education$grade_label]
df_individual_education <- df_individual_education %>% filter(round == TARGET_ROUND) %>% filter(!years_educ == 'NA')

df_personal_information <- df_personal_information %>% mutate(is_female = as.integer(sex_label == 'FEMALE'))
df_personal_information <- df_personal_information %>% mutate(is_female = as.integer(sex_label == 'FEMALE')) %>% filter(is_female == 1)

df <- df_individual_education %>% left_join(df_personal_information, by = c("round", "r_hhid", "r_id")) %>% filter(is_female == 1)


library(ggplot2)

SURVEY_YEAR <- 2008
REFORM_YEAR <- 1977
ENTRY_AGE   <- 7                       # official TZ primary entry age
ENTRY_YEAR  <- REFORM_YEAR - ENTRY_AGE # 1967 = birth year of cohort turning 7 in 1974
N           <- 5                      # window size, tweak as needed

df <- df %>%
  filter(!is.na(years_educ), !is.na(age)) %>%
  mutate(birth_year = SURVEY_YEAR - age)   # approx: ignores exact birth month

# --- 1. Average years of education by single-year age ---------------------
by_age <- df %>%
  group_by(age) %>%
  summarise(mean_educ = mean(years_educ), n = n(), .groups = "drop")

ggplot(by_age, aes(age, mean_educ)) +
  geom_point(aes(size = n), alpha = 0.6) +
  geom_smooth(se = FALSE, span = 0.3) +
  labs(title = "Average years of education by age (women, 2008)",
       x = "Age in 2008", y = "Mean years of education") +
  theme_minimal()

# --- 2. Zoom on birth cohorts around the reform year (1974) ---------------
by_birth_reform <- df %>%
  filter(birth_year >= REFORM_YEAR - N, birth_year <= REFORM_YEAR + N) %>%
  group_by(birth_year) %>%
  summarise(mean_educ = mean(years_educ), n = n(), .groups = "drop")

ggplot(by_birth_reform, aes(birth_year, mean_educ)) +
  geom_point(aes(size = n)) +
  geom_smooth(se = FALSE) +
  geom_vline(xintercept = REFORM_YEAR, linetype = "dashed", color = "red") +
  labs(title = sprintf("Education by birth year, %d–%d", REFORM_YEAR - N, REFORM_YEAR + N),
       subtitle = "Dashed line = Musoma Resolution (1974)",
       x = "Birth year", y = "Mean years of education") +
  theme_minimal()

# --- 3. Zoom on cohorts around primary-entry age at the reform ------------
by_birth_entry <- df %>%
  filter(birth_year >= ENTRY_YEAR - N, birth_year <= ENTRY_YEAR + N) %>%
  group_by(birth_year) %>%
  summarise(mean_educ = mean(years_educ), n = n(), .groups = "drop")x

ggplot(by_birth_entry, aes(birth_year, mean_educ)) +
  geom_point(aes(size = n)) +
  geom_smooth(se = FALSE) +
  geom_vline(xintercept = ENTRY_YEAR, linetype = "dashed", color = "red") +
  labs(title = sprintf("Education by birth year, %d–%d", ENTRY_YEAR - N, ENTRY_YEAR + N),
       subtitle = sprintf("Dashed line = cohort turning %d in %d (born %d)", ENTRY_AGE, REFORM_YEAR, ENTRY_YEAR),
       x = "Birth year", y = "Mean years of education") +
  theme_minimal()

hist(df$age, breaks=100)
?hist
