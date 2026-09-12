# === Load libraries
library(AER)
library(stargazer)
library(dplyr)
library(sandwich)
library(tidyr)



# === Load the data across all CSVs

# Dependent/outcome
df_views_on_violence = read.csv("data/upd4_hh_y.csv")

# Variable of interest
df_individual_education = read.csv("data/upd4_hh_c.csv")

# Controls
df_personal_information = read.csv("data/upd4_hh_b.csv")
df_location = read.csv("data/upd4_hh_a.csv")
df_religion = read.csv("data/upd4_hh_x1.csv")
df_alcohol = read.csv("data/upd4_hh_f.csv")
df_wealth = read.csv("data/upd4_hh_m.csv")


# === Extract the columns we actually want

# These are on a HOUSEHOLD level (UPHI)
df_views_on_violence <- df_views_on_violence %>% select(UPHI, round, r_hhid, r_id, hy_02_a, hy_02_b, hy_02_c, hy_02_d, hy_02_e, hy_02_f, hy_02_g, hy_02_h)
df_location <- df_location %>% select(UPHI, round, r_hhid, urban_label = urb_rur)
df_religion <- df_religion %>% select(UPHI, round, r_hhid, religion_label = hx_10)
df_wealth <- df_wealth %>% select(UPHI, round, r_hhid, item_label = hm_00, amount = hm_01, buy_price = hm_03, sell_price = hm_04)


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

# Because fathers education is very poorly reported, find the highest education that they submitted across any wave. This could see overestiamtes
# So measurement error is defiantely an issue, but its much better than everybody saying 'i dont know'.
df_personal_information <- df_personal_information %>% mutate(fathers_educ_na = ifelse(fathers_educ == -1, NA, fathers_educ)) %>%
                                                       group_by(UPI) %>%
                                                       mutate(fathers_educ_filled = suppressWarnings(max(fathers_educ_na, na.rm = TRUE))) %>%
                                                       ungroup() %>%
                                                       mutate(fathers_educ_filled = ifelse(is.infinite(fathers_educ_filled), -1, fathers_educ_filled)) %>%
                                                       select(-fathers_educ_na)


# To work out their wealth, add the average price of each item and multiply by the quantity
# Some products dont have price data, so use a reasonable fallback
# Take the buy_price and sell_price across all waves and just average these
# Yes, this fails to adjust for CPI and whatnot, but that error is tiny compared to the other measurement error here
asset_value_fallback <- c(
  "COOKING POTS, CUPS, OTHER KITCHEN UTENCILS" = 10000,
  "BOOKS (NOT SCHOOL BOOKS)" = 3000,
  "FIELDS/LAND" = 100000,
  "HOES" = 10000,
  "CUPBOARDS, CHEST-OF-DRAWERS, BOXES, WARDROBES,BOOKCASES" = 20000,
  "LANTERNS" = 5000,
  "FERTILIZER DISTRIBUTOR" = 100000
)

# When agging up the prices, anything 0 or NA will be treated as incorrectly reported and ignored
price_summary <- df_wealth %>% select(item_label, buy_price, sell_price) %>%
                               pivot_longer(cols = c(buy_price, sell_price), names_to = "type", values_to = "price") %>%
                               filter(!is.na(price) & price > 0) %>%
                               group_by(item_label) %>%
                               summarise(avg_price = median(price, na.rm = TRUE), n_obs = n(), .groups = "drop")

# From our table, build a map out of them to apply to all the rows
asset_value_map <- setNames(price_summary$avg_price, price_summary$item_label)

# From testing we know a few of these didn't have any price data so just manually append some guesstimates
missing_labels <- setdiff(names(asset_value_fallback), names(asset_value_map))
asset_value_map[missing_labels] <- asset_value_fallback[missing_labels]

# Finally, save this an normalise to 10k TSh
df_wealth$price <- asset_value_map[df_wealth$item_label]
df_wealth <- df_wealth %>% mutate(value = price * amount)

# Need to get rid of some crazy outliers that blow up house prices and what not
df_wealth <- df_wealth %>% group_by(UPHI, round, r_hhid) %>% 
                           mutate(total_wealth = sum(value, na.rm = TRUE)) %>%
                           select(UPHI, round, r_hhid, total_wealth) %>%
                           mutate(total_wealth = total_wealth / 1000000) %>%
                           ungroup()

# Map variables to a 1 or 0
df_personal_information <- df_personal_information %>% mutate(is_female = as.integer(sex_label == 'FEMALE')) %>% 
                                                       mutate(is_polygamous = as.integer(marriage_label == 'POLYGAMOUS MARRIED')) %>%
                                                       mutate(birth_year = 2008 - age)

df_location <- df_location %>% mutate(is_urban = as.integer(urban_label == 'URBAN'))


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
                                  left_join(df_religion, by = c("round", "r_hhid", "UPHI"), relationship = "many-to-many") %>%
                                  left_join(df_wealth, by = c("round", "r_hhid", "UPHI"), relationship = "many-to-many") 


# Then kill off all the rows that didn't merge or are unwanted
df <- df %>% filter(!supports_violence == 'NA') %>% 
             filter(is_female == 1) %>% 
             filter(fathers_educ_filled >= 0) %>% 
             filter(birth_year > 1950) %>%
             filter(total_wealth > 0 & !is.na(total_wealth))



df <- distinct(df)


# === Simple OLS regression
ols <- lm(supports_violence ~ years_educ + age + is_urban + is_polygamous + is_muslim + is_christian + drank_alcohol + total_wealth, data = df)
se_ols <- sqrt(diag(vcovHC(ols, type = "HC1")))

# Since there is likely a lot of intra-household dependence let's make sure we cluster our SE correctly on UPHI
se_ols_clustered <- sqrt(diag(vcovCL(ols, cluster = ~ UPHI)))

stargazer(
  ols, ols,
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
    "Drank alcohol (1/0)",
    "Wealth (1M TSh)"
  ),
  se = list(se_ols, se_ols_clustered),
  digits = 3,
  notes = "HC standard errors are reported in parentheses.",
  notes.append = TRUE
)


# === Summary stats
col <- "supports_violence"
df %>% summarise(min = min(.data[[col]]), mean = mean(.data[[col]]), max = max(.data[[col]]), sd = sd(.data[[col]]))

cols <- c("supports_violence", "years_educ", "age", "is_urban", "is_muslim", 
          "is_christian", "is_polygamous", "drank_alcohol", "total_wealth")

summary_stats <- df %>%
  summarise(across(all_of(cols), list(min = min, mean = mean, max = max, sd = sd),
                   .names = "{.col}__{.fn}")) %>%
  pivot_longer(everything(), names_to = c("col", "stat"), names_sep = "__",
               values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value)

print(summary_stats)


# === IV and 2SLS analysis

# This is the only one that runs across the entire period
iv_father <- ivreg(supports_violence ~ years_educ + age + is_urban + is_polygamous + is_muslim + is_christian + drank_alcohol + total_wealth | 
                          fathers_educ_filled + age + is_urban + is_polygamous + is_muslim + is_christian + drank_alcohol + total_wealth,
                        data = df_musoma)
se_iv_father <- sqrt(diag(vcovHC(iv_father, type = "HC1")))



# Reform is the Musoma Resolution in 1974-1977 which made Universal Primary Education much more accessible
# This was officially launched in 1977, we use this as an IV for education
# Since people enter primary when they are 7, consider those born in 1970 as when the regime shift occurs

YEAR_OF_REFORM = 1977
YEAR_OF_SURVEY = 2008
WINDOW = 3
PRIMARY_SCHOOL_AGE = 7

df_musoma <- df

# Work out whether the reform affected them (starting school after)
# Only look within a small window either side
df_musoma <- df_musoma  %>% mutate(affected_by_reform = as.integer(birth_year >= YEAR_OF_REFORM - PRIMARY_SCHOOL_AGE)) %>%
                            filter(birth_year >= YEAR_OF_REFORM - PRIMARY_SCHOOL_AGE - WINDOW) %>%
                            filter(birth_year <= YEAR_OF_REFORM - PRIMARY_SCHOOL_AGE + WINDOW)

# We're gonna run 4 regressions in this time period, OLS, each IV, IVs together
ols_1970 <- lm(supports_violence ~ years_educ + age + is_urban + is_polygamous + is_muslim + is_christian + drank_alcohol + total_wealth, data = df_musoma)
se_ols_1970 <- sqrt(diag(vcovHC(ols, type = "HC1")))

iv_both <- ivreg(supports_violence ~ years_educ + age + is_urban + is_polygamous + is_muslim + is_christian + drank_alcohol + total_wealth | 
                                       affected_by_reform + fathers_educ_filled + age + is_urban + is_polygamous + is_muslim + is_christian + drank_alcohol + total_wealth,
                                       data = df_musoma)
se_iv_both <- sqrt(diag(vcovHC(iv_both, type = "HC1")))


iv_father_1970 <- ivreg(supports_violence ~ years_educ + age + is_urban + is_polygamous + is_muslim + is_christian + drank_alcohol + total_wealth | 
                   fathers_educ_filled + age + is_urban + is_polygamous + is_muslim + is_christian + drank_alcohol + total_wealth,
                   data = df_musoma)
se_iv_father_1970 <- sqrt(diag(vcovHC(iv_father_1970, type = "HC1")))

iv_musoma_1970 <- ivreg(supports_violence ~ years_educ + age + is_urban + is_polygamous + is_muslim + is_christian + drank_alcohol + total_wealth | 
                        affected_by_reform + age + is_urban + is_polygamous + is_muslim + is_christian + drank_alcohol + total_wealth,
                        data = df_musoma)
se_iv_musoma_1970 <- sqrt(diag(vcovHC(iv_musoma_1970, type = "HC1")))


stargazer(
  ols_1970, iv_musoma_1970, iv_father_1970, iv_both,
  type = "text",
  title = "Table: OLS vs. 2SLS Results in Musoma Time Window",
  dep.var.labels = "Support of intimate partner violence",
  column.labels = c("OLS", "IV: Reform", "IV: Father's Educ", "IV: Both"),
  covariate.labels = c(
    "Formal education (years)",
    "Age (years)",
    "Urban (1/0)",
    "Polygamous (1/0)",
    "Muslim (1/0)",
    "Christian (1/0)",
    "Drank alcohol (1/0)",
    "Wealth (1M TSh)"
  ),
  se = list(se_ols_1970, se_iv_musoma_1970, se_iv_father_1970, se_iv_both),
  digits = 3,
  notes = "HC standard errors are reported in parentheses. All models restricted to the Musoma time window.",
  notes.append = TRUE
)

summary(iv_both, diagnostics = TRUE)
summary(iv_father_1970, diagnostics = TRUE)
summary(iv_musoma_1970, diagnostics = TRUE)


# === Diagnostics

# Quick fn to check the ICC within households.
# Many of them are very high (is_urban, religion etc), so this shows why clustering was needed
# These values are probably inflated by many clusters only having one member
icc_check <- function(var, data) {
  m <- lm(as.formula(paste(var, "~ 1")), data = data)
  aov_fit <- aov(as.formula(paste(var, "~ factor(UPHI)")), data = data)
  ss <- summary(aov_fit)[[1]]
  between <- ss["factor(UPHI)", "Sum Sq"]
  total <- between + ss["Residuals", "Sum Sq"]
  between / total
}

vars <- c("years_educ", "age", "is_urban", "is_muslim", "is_christian", 
          "is_polygamous", "drank_alcohol", "total_wealth")
icc_results <- sapply(vars, icc_check, data = df)
print(icc_results)