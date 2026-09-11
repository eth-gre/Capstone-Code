# === Load libraries
library(AER)
library(stargazer)
library(dplyr)

df_test = read.csv("data/upd4_hh_m.csv")
df_test <- df_test %>% select(UPHI, round, r_hhid, item_label = hm_00, amount = hm_01, buy_price = hm_03, sell_price = hm_04) %>%
                       filter(item_label == "HOUSE(S)")

plot(df_test$sell_price)
