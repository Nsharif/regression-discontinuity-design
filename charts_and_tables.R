##### Author: Naveed Sharif
##### Data Source: Kaiser Permanente
##### Topic: Regression Discontinuity Design
##### Date: 9/14/2018

### load dependent libraries
library(tidyverse)
library(data.table)
library(dummies)
library(gridExtra)
library(grid)
library(stargazer)

## Chart on average price by kp product
ggplot(data = kp_product_prices, mapping = aes(x=product, y=product_avg_price)) +
  geom_boxplot() + 
  ggtitle("Chart 1: Average Rate Breakdown by KP Product") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("KP Product") + ylab("KP Rate") +
  theme_set(theme_grey(base_size = 14)) 

## Table view of the dataframe
table_dataframe <- df2_eda %>%
  select(region_account_number, effective_date_year, kp_hdhp_fv) %>%
  filter(effective_date_year != 2012 & effective_date_year != 2013) %>%
  head(13)

grid.table(table_dataframe)

# Table of Regression Models
# summary table of regression model results
stargazer(df2_model_1
          ,df2_model_2
          ,df2_model_3
          ,df2_model_4
          ,df2_model_5
          ,type = "text"
          ,dep.var.labels = ""
          ,title = "Table 1: Regression Discontinuity Design Model Results"
          ,out = "model2.txt"
          ,digits = 2
          ,covariate.labels = c("KP hdhp Forcing Variable"
                                ,"KP Offers dhmo"
                                ,"KP Offers hmo"
                                ,"Average Age"
                                ,"Average KP Rate Increase"
                                ,"Prior Membership Growth"
                                ,"Trend"
                                ,"Years KP hdhp Offered"))

# Table of Regression Models for interaction terms
# summary table of regression model results
stargazer(df2_model_6
          ,df2_model_7
          ,type = "text"
          ,dep.var.labels = ""
          ,title = "Table 1: Regression Discontinuity Design Model Results"
          ,out = "model2.txt"
          ,digits = 2
          ,covariate.labels = c("KP hdhp fv"
                                ,"KP Offers dhmo"
                                ,"KP Offers hmo"
                                ,"Average Age"
                                ,"Average KP Rate Increase"
                                ,"Prior Membership Growth"
                                ,"Trend"
                                ,"Years KP hdhp Offered"
                                ,"KP parity to negative"
                                ,"KP parity to negative*KP hdhp fv"
                                ,"KP parity to positive"
                                ,"KP parity to positive*KP hdhp fv"))



