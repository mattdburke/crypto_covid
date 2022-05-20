### Title: R code for cryptocurrency paper
### Author: Matt Burke, John Fry, Sean Kemp and Drew Woodhouse
### Code editors: Drew Woodhouse and Matt Burke
### Last edited: 06/12/2021
### Description: Basic statistical analysis 
### Version 2

### Load libraries
library(tseries)
library(dplyr)
library(lubridate)
library(urca)
library(forecast)
library(tidyverse)
library(vars)
library(ggpubr)
library(zoo)
library(rmgarch)
library(quantmod)
library(DescTools)
library(lmtest)
library(sandwich)
library(gridExtra)
library(effects)

### Set directory
# setwd("C:/Users/mattb/Desktop/covid-crypto")
setwd("C:/Users/mattb/OneDrive - Sheffield Hallam University/Project covid-crypto")

btc <- read.csv("BTC-USD1.csv")
eth <- read.csv("ETH-USD.csv")
covid <- read.csv("owid-covid-data_update.csv")
cci <- read.csv("cci30_OHLCV.csv")
G <- read.csv("multiTimeline.csv")

### Produce global summaries for Covid-19 data and merge with crypto
### We use country level data, and aggregate it.
global_covid <- as.data.frame(covid %>%
	mutate(Date = as.Date(date)) %>%
	group_by(Date) %>%
	summarize(global_total_cases = sum(total_cases, na.rm=TRUE),
		global_total_deaths = sum(total_deaths, na.rm=TRUE),
		global_new_cases = sum(new_cases, na.rm=TRUE),
		global_new_deaths = sum(new_deaths, na.rm=TRUE),
		mean_policy_response = mean(stringency_index, na.rm=TRUE)
		))
eth$Date <- as.Date(eth$Date, "%Y-%m-%d")
btc$Date <- as.Date(btc$Date, "%Y-%m-%d")
global_covid$Date <- as.Date(global_covid$Date)
cci$Date <- as.Date(cci$Date)
cci$Open <- NULL
cci$High <- NULL
cci$Low <- NULL
eth$date <- NULL
#colnames(eth) <- c("Eth", "Date")
colnames(global_covid) <- c("Date", 
	"total_cases", 
	"total_deaths",
	"new_cases",
	"new_deaths",
	"mean_policy_response")
colnames(cci) <- c("Date", "Index", "Index_Volume")

eth <- dplyr::select(eth, Date, Eth = Adj.Close)
btc <- dplyr::select(btc, Date, ClosingPrice.USD. = Adj.Close)

df1 <- inner_join(eth, global_covid, by=c("Date"))
df2 <- inner_join(df1, btc, by=c("Date"))
df2 <- inner_join(df2, cci, by=c("Date"))


RetList <- function(x){
	n <- length(x)
	t <- x[1]
	T <- x[n]
	R = (T-t)/t
	return (R)
}


### Create all variables and consistent dataframe
df2 <- as.data.frame(df2 %>% mutate(

# one day lagged rolling week sum of cases. i.e. Monday to Sunday sum, on the row of the following Monday
Prior_7_cases = lag(rollapply(new_cases, width=7, sum, fill=NA, align='right')), 
Prior_7_deaths = lag(rollapply(new_deaths, width=7, sum, fill=NA, align='right')),
Prior_7_policy = lag(rollapply(mean_policy_response, width=7, sum, fill=NA, align='right')),

Prior_14_cases = lag(rollapply(new_cases, width=14, sum, fill=NA, align='right')), 
Prior_14_deaths = lag(rollapply(new_deaths, width=14, sum, fill=NA, align='right')),
Prior_14_policy = lag(rollapply(mean_policy_response, width=14, sum, fill=NA, align='right')),

# last three weeks mean, lagged by a week. First 3 weeks mean in a month, given at the end of the month
new_policy_long = lag(rollapply(mean_policy_response, 28, mean, fill=NA, align='right'),8),
# Last week lagged by a day
new_policy_short = lag(rollapply(mean_policy_response, 7, mean, fill=NA, align='right')),
AbPolicy = new_policy_short - new_policy_long,

new_policy_longx = lag(rollapply(mean_policy_response, 56, mean, fill=NA, align='right'),15),
# Last week lagged by a day
new_policy_shortx = lag(rollapply(mean_policy_response, 14, mean, fill=NA, align='right')),
AbPolicy_14 = new_policy_short - new_policy_long,

# Last week return, lagged by a day
Last_Week_Eth_Ret = lag(rollapply(Eth, width=7, RetList , fill=NA, align='right')),
Last_Week_Btc_Ret = lag(rollapply(ClosingPrice.USD., width=7, RetList, fill=NA, align='right')),
Last_Week_Index_Ret = lag(rollapply(Index, width=7, RetList, fill=NA, align='right')),

Last_TwoWeek_Eth_Ret = lag(rollapply(Eth, width=14, RetList , fill=NA, align='right')),
Last_TwoWeek_Btc_Ret = lag(rollapply(ClosingPrice.USD., width=14, RetList, fill=NA, align='right')),
Last_TwoWeek_Index_Ret = lag(rollapply(Index, width=14, RetList, fill=NA, align='right')),

# Forward looking weekly return, above variable reordered in time
Next_Week_Eth_Ret = lead(Last_Week_Eth_Ret, 8),
Next_Week_Btc_Ret = lead(Last_Week_Btc_Ret, 8),
Next_Week_Index_Ret = lead(Last_Week_Index_Ret, 8),

Next_TwoWeek_Eth_Ret = lead(Last_Week_Eth_Ret, 15),
Next_TwoWeek_Btc_Ret = lead(Last_Week_Btc_Ret, 15),
Next_TwoWeek_Index_Ret = lead(Last_Week_Index_Ret, 15)

))


df2 <- as.data.frame(df2 %>% mutate(
# Scale all variables
	Z_Prior_7_cases = scale(Prior_7_cases),
	Z_Prior_7_deaths = scale(Prior_7_deaths),

	Z_Prior_14_cases = scale(Prior_7_cases),
	Z_Prior_14_deaths = scale(Prior_7_deaths),

	Z_AbPolicy = scale(AbPolicy),
	Z_AbPolicy_14 = scale(AbPolicy_14),

	Z_ret_eth = scale(Last_Week_Eth_Ret),
	Z_ret_btc = scale(Last_Week_Btc_Ret),
	Z_ret_index = scale(Last_Week_Index_Ret),
	Z_ret_eth_14 = scale(Last_TwoWeek_Eth_Ret),
	Z_ret_btc_14 = scale(Last_TwoWeek_Btc_Ret),
	Z_ret_index_14 = scale(Last_TwoWeek_Index_Ret),
	ret_nextweek_eth = Next_Week_Eth_Ret*100,
	ret_nextweek_btc = Next_Week_Btc_Ret*100,
	ret_nextweek_index = Next_Week_Index_Ret*100,
	ret_nextweek_eth_14 = Next_TwoWeek_Eth_Ret*100,
	ret_nextweek_btc_14 = Next_TwoWeek_Btc_Ret*100,
	ret_nextweek_index_14 = Next_TwoWeek_Index_Ret*100,
	))
	
df2 <- df2[complete.cases(df2),]

df2 <- dplyr::filter(df2, Date<"2021-11-26")


fit <- lm(ret_nextweek_eth_14 ~ Z_Prior_14_cases*Z_AbPolicy_14 + Z_Prior_14_cases + Z_AbPolicy_14 + Z_ret_eth_14, data=df2)
coeftest(fit, vcov = NeweyWest(fit, lag = 14, prewhite = FALSE))
fit <- lm(ret_nextweek_btc_14 ~ Z_Prior_14_cases*Z_AbPolicy_14 + Z_Prior_14_cases + Z_AbPolicy_14 + Z_ret_btc_14, data=df2)
coeftest(fit, vcov = NeweyWest(fit, lag = 14, prewhite = FALSE))
fit <- lm(ret_nextweek_index_14 ~ Z_Prior_14_cases*Z_AbPolicy_14 + Z_Prior_14_cases + Z_AbPolicy_14 + Z_ret_index_14, data=df2)
coeftest(fit, vcov = NeweyWest(fit, lag = 14, prewhite = FALSE))

fit <- lm(ret_nextweek_eth_14 ~ Z_Prior_14_deaths*Z_AbPolicy_14 + Z_Prior_14_deaths + Z_AbPolicy_14 + Z_ret_eth_14, data=df2)
coeftest(fit, vcov = NeweyWest(fit, lag = 14, prewhite = FALSE))
fit <- lm(ret_nextweek_btc_14 ~ Z_Prior_14_deaths*Z_AbPolicy_14 + Z_Prior_14_deaths + Z_AbPolicy_14 + Z_ret_btc_14, data=df2)
coeftest(fit, vcov = NeweyWest(fit, lag = 14, prewhite = FALSE))
fit <- lm(ret_nextweek_index_14 ~ Z_Prior_14_deaths*Z_AbPolicy_14 + Z_Prior_14_deaths + Z_AbPolicy_14 + Z_ret_index_14, data=df2)
coeftest(fit, vcov = NeweyWest(fit, lag = 14, prewhite = FALSE))