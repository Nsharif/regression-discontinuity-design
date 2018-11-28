##### Author: Naveed Sharif
##### Data Source: Kaiser Permanente
##### Topic: Regression Discontinuity Design
##### Date: 9/14/2018

### load dependent libraries
library(tidyverse)
library(data.table)
library(dummies)

## evaluate average premium prices between products
kp_product_prices_1 <- filter(df_eda, kp_hmo == 1) %>%
  summarize(product_avg_price = mean(kp_rate_mean))
kp_product_prices_1$product <- 'hmo'

kp_product_prices_2 <- filter(df_eda, kp_dhmo == 1) %>%
  summarize(product_avg_price = mean(kp_rate_mean))
kp_product_prices_2$product <- 'dhmo'

kp_product_prices_3 <- filter(df_eda, kp_hdhp == 1) %>%
  summarize(product_avg_price = mean(kp_rate_mean))
kp_product_prices_3$product <- 'hdhp'

kp_product_prices <- rbind(kp_product_prices_1,kp_product_prices_2,kp_product_prices_3)
kp_product_prices$product <- as.factor(kp_product_prices$product)
table(kp_product_prices$product)

ggplot(data = kp_product_prices, mapping = aes(x=product, y=product_avg_price)) +
  geom_boxplot() + 
  ggtitle("Chart 1: Average Rate Breakdown by KP Product") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("KP Product") + ylab("KP Rate") +
  theme_set(theme_grey(base_size = 14)) 

# Distinct count of groups in analysis. 146 groups
df2_eda %>%
  group_by(region_account_number) %>%
  summarize(Unique_Elements = n_distinct(region_account_number)) 