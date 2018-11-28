##### Author: Naveed Sharif
##### Data Source: Kaiser Permanente
##### Topic: Regression Discontinuity Design
##### Date: 9/14/2018

### load dependent libraries
library(tidyverse)
library(data.table)
library(dummies)

## load dataframe into enviornment
df_eda <- read.csv("C:\\Users\\Naveed\\Desktop\\Employment\\Kaiser\\R Projects\\Regression Discontinuity\\KP_NationalAccount.csv", stringsAsFactors = TRUE)
options(scipen=999)
str(df_eda, list.len=200)

## create forcing variable column.
## to reduce confusion. kp_hdhp_fv is used as the forcing variable in this discontinuity design
df_eda$kp_hdhp_fv <- df_eda$kp_hdhp

## create columns needed for the discontinuity design
## first year hdhp was offered to kp
df_eda <- data.table(df_eda) 
setkey(df_eda,region_account_number)
df_eda[ , first_year_kp_hdhp := c(NA,diff(kp_hdhp,1)), by=region_account_number]
setDF(df_eda)
str(df_eda)

## between years 2012 through 2018, 2015 had the highest count of kp groups that had an 
## hdhp offerering alongside another kp product
a <- filter(df_eda, first_year_kp_hdhp == 1, effective_date_year == 2012) # 0 groups
b <- filter(df_eda, first_year_kp_hdhp == 1, effective_date_year == 2013) # 13 groups
c <- filter(df_eda, first_year_kp_hdhp == 1, effective_date_year == 2014) # 33 groups
d <- filter(df_eda, first_year_kp_hdhp == 1, effective_date_year == 2015) # 33 groups
e <- filter(df_eda, first_year_kp_hdhp == 1, effective_date_year == 2016) # 38 groups
f <- filter(df_eda, first_year_kp_hdhp == 1, effective_date_year == 2017) # 22 groups
g <- filter(df_eda, first_year_kp_hdhp == 1, effective_date_year == 2018) # 07 groups

## kp hdhp alongside another kp product
## a=0, b=11, c=16, d=24, e=15, f=7, g=3
filter(d, kp_hmo == 1 | kp_dhmo ==1)

remove(a)
remove(b)
remove(c)
remove(d)
remove(e)
remove(f)
remove(g)

## create membership growth in count and percentage
df_eda <- data.table(df_eda)
setkey(df_eda,region_account_number)
df_eda[ , mg := c(NA,diff(n_subscribers,1)), by=region_account_number]
setDF(df_eda)
str(df_eda)

df_eda <- df_eda %>%
  group_by(region_account_number) %>%
  mutate(mg_pct = n_subscribers/lag(n_subscribers)-1)

df_eda$mg_pct <- round(df_eda$mg_pct,3)
str(df_eda)

## create kp offers hdhp (static throughout all years)  
z <- df_eda %>%
  group_by(region_account_number) %>%
  summarize(total_years_hdhp_offered = sum(kp_hdhp))

z$kp_hdhp <- ifelse(z$total_years_hdhp_offered >= 1,1,0)

df_eda <- within(df_eda,remove(kp_hdhp))
df_eda <- left_join(df_eda,z,by='region_account_number')
str(df_eda)
remove(z)

## constructing the fv design. within a group it has to have a zero and a one for the fv to work 
## reducing observations to groups only offered an hdhp product
df2_eda <- filter(df_eda, kp_hdhp==1)

df2_eda <- df2_eda %>%
  group_by(region_account_number) %>%
  mutate(c = total_years_hdhp_offered - 1) %>%
  mutate(n = length(region_account_number)) %>%
  mutate(d = n - total_years_hdhp_offered)

df2_eda <- filter(df2_eda, d != 0 & d != 1) # validate this is the bandwidth size

## create centered forcing variable
a <- filter(df2_eda, kp_hdhp_fv == 0)
b <- filter(df2_eda, kp_hdhp_fv == 1)

b <- b %>%
  group_by(region_account_number) %>%
  mutate(kp_hdhp_cfv = seq(n())-1)

a <- a %>%
  group_by(region_account_number) %>%
  mutate(kp_hdhp_cfv = rev(seq(n()))*-1)

df2_eda <- rbind(a,b)

df2_eda <- df2_eda %>% arrange(region_account_number, effective_date_year)

df2_eda <- df2_eda %>% 
  group_by(region_account_number)

remove(a)
remove(b)

## create dummy variables for the effective year.
df2_eda <- as.data.frame(df2_eda)
df2_eda <- cbind(df2_eda,dummy(df2_eda$effective_date_year, sep = "_"))
str(df2_eda)

## create contribution state bin variables
df2_eda$cp_mean_state <- ifelse(df2_eda$cp_mean > 40, 'very_positive',
                          ifelse((df2_eda$cp_mean > 20 & df2_eda$cp_mean <= 40),'positive',
                                  ifelse((df2_eda$cp_mean > -20 & df2_eda$cp_mean <= 20),'parity',
                                        ifelse((df2_eda$cp_mean > -40 & df2_eda$cp_mean <= -20),'negative','very_negative'))))

prop.table(table(df2_eda$cp_mean_state))

## create change in contribution state bin variables
df2_eda <- df2_eda %>%
  group_by(region_account_number) %>%
  mutate(cp_state_n_to_pa = ifelse((lag(cp_mean_state)=='very_negative' | lag(cp_mean_state)=='negative') 
                                    & cp_mean_state == 'parity',1,0))

df2_eda <- df2_eda %>%
  group_by(region_account_number) %>%
  mutate(cp_state_n_to_po = ifelse((lag(cp_mean_state)=='very_negative' | lag(cp_mean_state)=='negative') 
                                   & (cp_mean_state == 'positive' | cp_mean_state == 'very_positve'),1,0))

df2_eda <- df2_eda %>%
  group_by(region_account_number) %>%
  mutate(cp_state_pa_to_po = ifelse(lag(cp_mean_state)=='parity' 
                                   & (cp_mean_state == 'positive' | cp_mean_state == 'very_positive'),1,0))

df2_eda <- df2_eda %>%
  group_by(region_account_number) %>%
  mutate(cp_state_pa_to_n = ifelse(lag(cp_mean_state)=='parity' 
                                    & (cp_mean_state == 'negative' | cp_mean_state == 'very_negative'),1,0))

df2_eda <- df2_eda %>%
  group_by(region_account_number) %>%
  mutate(cp_state_po_to_pa = ifelse((lag(cp_mean_state)=='very_positve' | lag(cp_mean_state)=='positive') 
                                   & cp_mean_state == 'parity',1,0))

df2_eda <- df2_eda %>%
  group_by(region_account_number) %>%
  mutate(cp_state_po_to_n = ifelse((lag(cp_mean_state)=='very_positve' | lag(cp_mean_state)=='positive') 
                                   & (cp_mean_state == 'negative' | cp_mean_state == 'very_negative'),1,0))

df2_eda$cp_mean_state_bins <- NA
df2_eda$cp_mean_state_bins[df2_eda$cp_state_n_to_pa == 1] <- 'n_to_pa'
df2_eda$cp_mean_state_bins[df2_eda$cp_state_n_to_po == 1] <- 'n_to_po'
df2_eda$cp_mean_state_bins[df2_eda$cp_state_pa_to_po == 1] <- 'pa_to_po'
df2_eda$cp_mean_state_bins[df2_eda$cp_state_pa_to_n == 1] <- 'pa_to_n'
df2_eda$cp_mean_state_bins[df2_eda$cp_state_po_to_pa == 1] <- 'po_to_pa'
df2_eda$cp_mean_state_bins[df2_eda$cp_state_po_to_n == 1] <- 'po_to_n'


