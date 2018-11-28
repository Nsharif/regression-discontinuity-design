##### Author: Naveed Sharif
##### Data Source: Kaiser Permanente
##### Topic: Regression Discontinuity Design
##### Date: 9/14/2018

### load dependent libraries
library(tidyverse)
library(data.table)
library(dummies)

## regression discontinuity design models
df2_model_1 <- lm(mg_pct
                  ~kp_hdhp_fv
                  ,data = df2_eda)

summary(df2_model_1)

df2_model_2 <- lm(mg_pct
                  ~kp_hdhp_fv
                  +kp_dhmo # identify that there is a difference in product mix
                  +kp_hmo # identify that there is a difference in product mix
                  ,data = df2_eda)

summary(df2_model_2)

df2_model_3 <- lm(mg_pct
                  ~kp_hdhp_fv
                  +kp_dhmo # identify that there is a difference in product mix
                  +kp_hmo # identify that there is a difference in product mix
                  +mrn_avg_age # identify that there is a difference in age
                  +ri_mean # identify that there is a difference in RI
                  ,data = df2_eda)

summary(df2_model_3)

df2_model_4 <- lm(mg_pct
                  ~kp_hdhp_fv
                  +kp_dhmo
                  +kp_hmo
                  +mrn_avg_age
                  +ri_mean
                  +lag(mg_pct)
                  ,data = df2_eda)

summary(df2_model_4)

df2_model_5 <- lm(mg_pct
                  ~kp_hdhp_fv
                  +kp_dhmo
                  +kp_hmo
                  +mrn_avg_age
                  +ri_mean
                  +lag(mg_pct)
                  +effective_date_year
                  +total_years_hdhp_offered
                  ,data = df2_eda)

summary(df2_model_5)


df2_model_6 <- lm(mg_pct
                  ~kp_hdhp_fv
                  +kp_dhmo
                  +kp_hmo
                  +mrn_avg_age
                  +ri_mean
                  +lag(mg_pct)
                  +effective_date_year
                  +total_years_hdhp_offered
                  +cp_state_pa_to_n
                  +cp_state_pa_to_n:kp_hdhp_fv
                  ,data = df2_eda)

summary(df2_model_6)

df2_model_7 <- lm(mg_pct
                  ~kp_hdhp_fv
                  +kp_dhmo
                  +kp_hmo
                  +mrn_avg_age
                  +ri_mean
                  +lag(mg_pct)
                  +effective_date_year
                  +total_years_hdhp_offered
                  +cp_state_pa_to_po
                  +cp_state_pa_to_po:kp_hdhp_fv
                  ,data = df2_eda)

summary(df2_model_7)

