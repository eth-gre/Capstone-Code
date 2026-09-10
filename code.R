# === Load libraries
library(AER)
library(stargazer)
library(dplyr)



# === Load the data across all CSVs

# Dependent/outcome
df_views_on_violence = read.csv("data/upd4_hh_y.csv")

# Variable of interest
df_individual_education = read.csv("data/upd4_hh_c.csv")

# Controls
df_personal_information = read.csv("data/upd4_hh_b.csv")
df_location = read.csv("data/upd4_hh_a.csv")
df_expenditure = read.csv("data/upd4_hh_l.csv")
df_religion = read.csv("data/upd4_hh_x1.csv")
df_alcohol = read.csv("data/upd4_hh_f.csv")


# === Extract the columns we actually want

# These are on a HOUSEHOLD level (UPHI)
df_views_on_violence <- df_views_on_violence %>% select(UPHI, round, r_hhid, r_id, hy_02_a, hy_02_b, hy_02_c, hy_02_d, hy_02_e, hy_02_f, hy_02_g, hy_02_h)
df_location <- df_location %>% select(UPHI, round, r_hhid, urban_label = urb_rur)
df_expenditure <- df_expenditure %>% select(UPHI, round, r_hhid, expense = hl_02)
df_religion <- df_religion %>% select(UPHI, round, r_hhid, religion_label = hx_10)


# These are on an INDIVIDUAL level (UPI)
df_individual_education <- df_individual_education %>% select(UPI, round, r_hhid, r_id, grade_label = hc_07, educ_cost = hc_28_8)
df_personal_information <- df_personal_information %>% select(UPI, round, r_hhid, r_id, sex_label = hb_02, age = hb_04, marriage_label = hb_19, fathers_educ_label = hb_14)
df_alcohol <- df_alcohol %>% select(UPI, round, r_hhid, r_id, alcohol_label = hf_08)


# Clean up the types (R thinks this is a number, silly R)
df_views_on_violence$r_hhid <- as.character(df_views_on_violence$r_hhid)
df_religion$r_hhid <- as.character(df_religion$r_hhid)



# === Create derived columns

# They 'support' violence if they answer yes to ANY of the questions
df_views_on_violence <- df_views_on_violence %>% mutate(supports_violence = as.integer(if_any(c(hy_02_a, hy_02_b, hy_02_c, hy_02_d, hy_02_e, hy_02_f, hy_02_g, hy_02_h), ~ .x == ' YES')))

# Education is provided as a label and must be converted to years for the regression
# Many of them are a lil bit ambiguous, soooo sorry to anyone who did a diploma??
# They get an 'NA' string which gets filtered out later
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

fathers_educ_years_map <- c(
  "NO SCHOOL" = 0,
  "DON'T KNOW" = 0,
  
  "SOME PRIMARY" = 3,
  "COMPLETED PRIMARY" = 6,
  "SOME SECONDARY" = 9,
  "COMPLETED SECONDARY" = 13,
  "MORE THAN SECONDARY" = 17
)

df_individual_education$years_educ <- educ_years_map[df_individual_education$grade_label]
df_personal_information$fathers_educ <- fathers_educ_years_map[df_personal_information$fathers_educ_label]

# Just write these as -1s to be dropped later
df_personal_information$fathers_educ[is.na(df_personal_information$fathers_educ)] <- -1

df_personal_information <- df_personal_information %>% mutate(fathers_educ_na = ifelse(fathers_educ == -1, NA, fathers_educ)) %>%
                                                       group_by(UPI) %>%
                                                       mutate(fathers_educ_filled = suppressWarnings(max(fathers_educ_na, na.rm = TRUE))) %>%
                                                       ungroup() %>%
                                                       mutate(fathers_educ_filled = ifelse(is.infinite(fathers_educ_filled), -1, fathers_educ_filled)) %>%
                                                       select(-fathers_educ_na)

# Map variables to a 1 or 0
df_personal_information <- df_personal_information %>% mutate(is_female = as.integer(sex_label == 'FEMALE')) %>% 
                                                       mutate(is_polygamous = as.integer(marriage_label == 'POLYGAMOUS MARRIED'))

df_location <- df_location %>% mutate(is_urban = as.integer(urban_label == 'URBAN'))

# For household expenditure, group and sum (then drop the individual expenses to help with the merge)
df_expenditure <- df_expenditure %>% group_by(UPHI, round, r_hhid) %>% mutate(total_expenses = sum(expense, na.rm = TRUE)) %>% ungroup() %>% 
                                     select(UPHI, round, r_hhid, total_expenses) %>% 
                                     mutate(total_expenses = total_expenses / 1000)


# Show religion through dummies belonging to the main ones. All others are too small to care
CHRISTIAN_LABELS = c('CATHOLIC', 'LUTHERANS', 'OTHER PROTESTANTS', 'OTHER CHRISTIANS')
MUSLIM_LABELS = c('MUSLIM')

df_religion <- df_religion %>% mutate(is_muslim = as.integer(religion_label %in% MUSLIM_LABELS)) %>% 
                               mutate(is_christian = as.integer(religion_label %in% CHRISTIAN_LABELS))

df_alcohol <- df_alcohol %>% mutate(drank_alcohol = as.integer(alcohol_label == 'YES')) 


# === Filter the data down

# Make everything round 1
TARGET_ROUND <- 1

df_views_on_violence <- df_views_on_violence %>% filter(round == TARGET_ROUND)
df_individual_education <- df_individual_education %>% filter(round == TARGET_ROUND)

# The individual needs and education label
df_individual_education <- df_individual_education %>% filter(!years_educ == 'NA')


# === Merge all of the CSVs together (on an individual level)
df <- df_individual_education %>% left_join(df_views_on_violence, by = c("round", "r_hhid", "r_id")) %>% 
                                  left_join(df_personal_information, by = c("round", "r_hhid", "r_id")) %>% 
                                  left_join(df_alcohol, by = c("round", "r_hhid", "r_id")) %>% 
                                  left_join(df_location, by = c("round", "r_hhid", "UPHI"), relationship = "many-to-many") %>% 
                                  left_join(df_expenditure, by = c("round", "r_hhid", "UPHI"), relationship = "many-to-many") %>% 
                                  left_join(df_religion, by = c("round", "r_hhid", "UPHI"), relationship = "many-to-many")

df <- df %>% mutate(birth_year = 2008 - age)

# Then kill off all the rows that didn't merge or are unwanted
df <- df %>% filter(!supports_violence == 'NA') %>% 
             filter(!is.na(total_expenses) & total_expenses > 0) %>% 
             filter(is_female == 1) %>% 
             filter(fathers_educ_filled >= 0) %>% 
             filter(birth_year > 1950)


df <- distinct(df)


# === Simple OLS regression
ols <- lm(supports_violence ~ years_educ + age + is_urban + is_polygamous + is_muslim + is_christian + drank_alcohol, data = df)
se_ols <- sqrt(diag(vcovHC(ols, type = "HC1")))

stargazer(
  ols,
  type = "text",
  title = "Table 1: OLS Regression Results",
  dep.var.labels = "Support of intimate partner violence",
  covariate.labels = c(
    "Formal education (years)",
    "Age (years)",
    "Urban (1/0)",
    "Polygamous (1/0)",
    "Muslim (1/0)",
    "Christian (1/0)",
    "Drank alcohol (1/0)"
  ),
  se = list(se_ols),
  digits = 3,
  notes = "HC standard errors are reported in parentheses.",
  notes.append = TRUE
)


# === Summary stats
col <- "supports_violence"
df %>% summarise(min = min(.data[[col]]), mean = mean(.data[[col]]), max = max(.data[[col]]), sd = sd(.data[[col]]))



# === IV and 2SLS analysis

# First reform is the Musoma Resolution in 1974-1977 which made Universal Primary Education much more accessible
# This was officially launched in 1977, so use this as an IV for education
# Since people enter primary when they are 7, consider those born in 1970 as the cutoff

YEAR_OF_REFORM = 1977
YEAR_OF_SURVEY = 2008
WINDOW = 5
PRIMARY_SCHOOL_AGE = 7

df_musoma <- df

# Work out whether the reform affected them (starting school after)
# Only look within a small window either side
df_musoma <- df_musoma  %>% mutate(affected_by_reform = as.integer(birth_year >= YEAR_OF_REFORM - PRIMARY_SCHOOL_AGE)) %>%
                            filter(birth_year >= YEAR_OF_REFORM - PRIMARY_SCHOOL_AGE - WINDOW) %>%
                            filter(birth_year <= YEAR_OF_REFORM - PRIMARY_SCHOOL_AGE + WINDOW)

iv_musoma <- ivreg(supports_violence ~ years_educ + age + is_urban + is_polygamous + is_muslim + is_christian + drank_alcohol | 
                                       affected_by_reform + fathers_educ_filled + age + is_urban + is_polygamous + is_muslim + is_christian + drank_alcohol,
                                       data = df_musoma)
se_iv_musoma <- sqrt(diag(vcovHC(iv_musoma, type = "HC1")))

stargazer(
  iv_musoma,
  type = "text",
  title = "Table 2: 2SLS Regression Results with Musoma Resolution as IV",
  dep.var.labels = "Support of intimate partner violence",
  covariate.labels = c(
    "Formal education (years)",
    "Age (years)",
    "Urban (1/0)",
    "Polygamous (1/0)",
    "Muslim (1/0)",
    "Christian (1/0)",
    "Drank alcohol (1/0)"
  ),
  se = list(se_iv_musoma),
  digits = 3,
  notes = "HC standard errors are reported in parentheses.",
  notes.append = TRUE
)

summary(iv_musoma, diagnostics = TRUE)

mean(df_musoma$affected_by_reform)

summary(lm(years_educ ~ affected_by_reform, data = df_musoma))
cor(df_musoma$age, df_musoma$affected_by_reform)

ols_1970 <- lm(supports_violence ~ years_educ + age + is_urban + is_polygamous + is_muslim + is_christian + drank_alcohol, data = df_musoma)
se_ols_1970 <- sqrt(diag(vcovHC(ols, type = "HC1")))

stargazer(
  ols_1970,
  type = "text",
  title = "Table 3: OLS Regression Results in Musoma Time Window",
  dep.var.labels = "Support of intimate partner violence",
  covariate.labels = c(
    "Formal education (years)",
    "Age (years)",
    "Urban (1/0)",
    "Polygamous (1/0)",
    "Muslim (1/0)",
    "Christian (1/0)",
    "Drank alcohol (1/0)"
  ),
  se = list(se_ols_1970),
  digits = 3,
  notes = "HC standard errors are reported in parentheses.",
  notes.append = TRUE
)

hist(df$birth_year, breaks=30)

plot(df$birth_year, df$supports_violence,
     pch = 16, col = adjustcolor("steelblue", alpha.f = 0.3),
     xlab = "Birth year", ylab = "Years of education",
     main = "Education vs. Birth Year")

# Binned means by birth year
binned <- aggregate(supports_violence ~ birth_year, data = df, FUN = mean)
lines(binned$birth_year, binned$supports_violence, col = "darkred", lwd = 2)
points(binned$birth_year, binned$supports_violence, col = "darkred", pch = 19)
