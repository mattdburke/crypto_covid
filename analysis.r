### Title: R code for cryptocurrency paper
### Author: Drew Woodhouse and Matt Burke
### Last edited: 06/12/2021
### Description: Basic statistical analysis 

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
setwd("C:/Users/mattb/Desktop/covid-crypto")

### Load hosted data
btc <- read.csv("https://mattdburke.github.io/data/BTC-USD.csv")
eth <- read.csv("https://mattdburke.github.io/data/eth.csv")
covid <- read.csv("https://mattdburke.github.io/data/owid-covid-data.csv")
cci <- read.csv("https://mattdburke.github.io/data/cci.csv")
G <- read.csv("https://mattdburke.github.io/data/google.csv")

### Produce global summaries for Covid-19 data and merge with crypto
global_covid <- as.data.frame(covid %>%
	mutate(Date = as.Date(date)) %>%
	group_by(Date) %>%
	summarize(global_total_cases = sum(total_cases, na.rm=TRUE),
		global_total_deaths = sum(total_deaths, na.rm=TRUE),
		global_new_cases = sum(new_cases, na.rm=TRUE),
		global_new_deaths = sum(new_deaths, na.rm=TRUE),
		mean_policy_response = mean(stringency_index, na.rm=TRUE)
		))
eth$Date <- as.Date(eth$date, "%d/%m/%Y")
btc$Date <- as.Date(btc$Date, "%d/%m/%Y")
global_covid$Date <- as.Date(global_covid$Date)
cci$Date <- as.Date(cci$Date)
cci$Open <- NULL
cci$High <- NULL
cci$Low <- NULL
eth$date <- NULL
colnames(eth) <- c("Eth", "Date")
colnames(global_covid) <- c("Date", 
	"total_cases", 
	"total_deaths",
	"new_cases",
	"new_deaths",
	"mean_policy_response")
colnames(cci) <- c("Date", "Index", "Index_Volume")
df1 <- inner_join(eth, global_covid, by=c("Date"))
df2 <- inner_join(df1, btc, by=c("Date"))
df2 <- inner_join(df2, cci, by=c("Date"))
df2$Currency <- NULL
df2$X24h.Open..USD. <- NULL
df2$X24h.High..USD. <- NULL
df2$X24h.Low..USD. <- NULL

### Create all variables and consistent dataframe
df2 <- as.data.frame(df2 %>% mutate(
	# produce all lags of Covid-19 data
	new_cases.lag14 = dplyr::lag(new_cases, 14),
	new_cases.lag13 = dplyr::lag(new_cases, 13),
	new_cases.lag12 = dplyr::lag(new_cases, 12),
	new_cases.lag11 = dplyr::lag(new_cases, 11),
	new_cases.lag10 = dplyr::lag(new_cases, 10),
	new_cases.lag9 = dplyr::lag(new_cases, 9),
	new_cases.lag8 = dplyr::lag(new_cases, 8),
	new_cases.lag7 = dplyr::lag(new_cases, 7),
	new_cases.lag6 = dplyr::lag(new_cases, 6),
	new_cases.lag5 = dplyr::lag(new_cases, 5),
	new_cases.lag4 = dplyr::lag(new_cases, 4),
	new_cases.lag3 = dplyr::lag(new_cases, 3),
	new_cases.lag2 = dplyr::lag(new_cases, 2),
	new_cases.lag1 = dplyr::lag(new_cases, 1),
	new_deaths.lag14 = dplyr::lag(new_deaths, 14),
	new_deaths.lag13 = dplyr::lag(new_deaths, 13),
	new_deaths.lag12 = dplyr::lag(new_deaths, 12),
	new_deaths.lag11 = dplyr::lag(new_deaths, 11),
	new_deaths.lag10 = dplyr::lag(new_deaths, 10),
	new_deaths.lag9 = dplyr::lag(new_deaths, 9),
	new_deaths.lag8 = dplyr::lag(new_deaths, 8),
	new_deaths.lag7 = dplyr::lag(new_deaths, 7),
	new_deaths.lag6 = dplyr::lag(new_deaths, 6),
	new_deaths.lag5 = dplyr::lag(new_deaths, 5),
	new_deaths.lag4 = dplyr::lag(new_deaths, 4),
	new_deaths.lag3 = dplyr::lag(new_deaths, 3),
	new_deaths.lag2 = dplyr::lag(new_deaths, 2),
	new_deaths.lag1 = dplyr::lag(new_deaths, 1),
	mean_policy_response.lag14 = dplyr::lag(mean_policy_response, 14),
	mean_policy_response.lag13 = dplyr::lag(mean_policy_response, 13),
	mean_policy_response.lag12 = dplyr::lag(mean_policy_response, 12),
	mean_policy_response.lag11 = dplyr::lag(mean_policy_response, 11),
	mean_policy_response.lag10 = dplyr::lag(mean_policy_response, 10),
	mean_policy_response.lag9 = dplyr::lag(mean_policy_response, 9),
	mean_policy_response.lag8 = dplyr::lag(mean_policy_response, 8),
	mean_policy_response.lag7 = dplyr::lag(mean_policy_response, 7),
	mean_policy_response.lag6 = dplyr::lag(mean_policy_response, 6),
	mean_policy_response.lag5 = dplyr::lag(mean_policy_response, 5),
	mean_policy_response.lag4 = dplyr::lag(mean_policy_response, 4),
	mean_policy_response.lag3 = dplyr::lag(mean_policy_response, 3),
	mean_policy_response.lag2 = dplyr::lag(mean_policy_response, 2),
	mean_policy_response.lag1 = dplyr::lag(mean_policy_response, 1),
	Prior_10_cases = new_cases.lag11+new_cases.lag10+new_cases.lag9+new_cases.lag8+new_cases.lag7+new_cases.lag6+new_cases.lag5+new_cases.lag4+new_cases.lag3+new_cases.lag2+new_cases.lag1,
	Prior_10_deaths = new_deaths.lag11+new_deaths.lag10+new_deaths.lag9+new_deaths.lag8+new_deaths.lag7+new_deaths.lag6+new_deaths.lag5+new_deaths.lag4+new_deaths.lag3+new_deaths.lag2+new_deaths.lag1,
	Prior_10_policy = mean_policy_response.lag11+mean_policy_response.lag10+mean_policy_response.lag9+mean_policy_response.lag8+mean_policy_response.lag7+mean_policy_response.lag6+mean_policy_response.lag5+mean_policy_response.lag4+mean_policy_response.lag3+mean_policy_response.lag2+mean_policy_response.lag1,
	Prior_7_cases = new_cases.lag7+new_cases.lag6+new_cases.lag5+new_cases.lag4+new_cases.lag3+new_cases.lag2+new_cases.lag1,
	Prior_7_deaths = new_deaths.lag7+new_deaths.lag6+new_deaths.lag5+new_deaths.lag4+new_deaths.lag3+new_deaths.lag2+new_deaths.lag1,
	Prior_14_cases = new_cases.lag14+new_cases.lag13+new_cases.lag12+new_cases.lag11+new_cases.lag10+new_cases.lag9+new_cases.lag8+new_cases.lag7+new_cases.lag6+new_cases.lag5+new_cases.lag4+new_cases.lag3+new_cases.lag2+new_cases.lag1,
	Prior_14_deaths = new_deaths.lag14+new_deaths.lag13+new_deaths.lag12+new_deaths.lag11+new_deaths.lag10+new_deaths.lag9+new_deaths.lag8+new_deaths.lag7+new_deaths.lag6+new_deaths.lag5+new_deaths.lag4+new_deaths.lag3+new_deaths.lag2+new_deaths.lag1,
	new_cases_long = rollapply(lag(new_cases, 6), 25, mean, fill=NA, alight='right'),
	new_cases_short = rollapply(lag(new_cases, 1), 5, mean, fill=NA, alight='right'),
	AbCases = new_cases_short - new_cases_long,
	new_deaths_long = rollapply(lag(new_deaths, 6), 25, mean, fill=NA, alight='right'),
	new_deaths_short = rollapply(lag(new_deaths, 1), 5, mean, fill=NA, alight='right'),
	AbDeaths = new_deaths_short - new_deaths_long,
	new_policy_long = rollapply(lag(mean_policy_response, 6), 25, mean, fill=NA, alight='right'),
	new_policy_short = rollapply(lag(mean_policy_response, 1), 5, mean, fill=NA, alight='right'),
	AbPolicy = new_policy_short - new_policy_long))
# Produce return data
df2 <- as.data.frame(df2 %>% mutate(
	ret_t_eth = Delt(Eth),
	ret_t_btc = Delt(ClosingPrice.USD.),
	ret_t_index = Delt(Index),
	eth.lag7 = dplyr::lag(Eth, 7),
	eth.lead1 = dplyr::lead(Eth, 1),
	eth.lead7 = dplyr::lead(Eth, 7),
	eth.lead14 = dplyr::lead(Eth, 14),
	eth.lead21 = dplyr::lead(Eth, 21),
	btc.lag7 = dplyr::lag(ClosingPrice.USD., 7),
	btc.lead1 = dplyr::lead(ClosingPrice.USD., 1),
	btc.lead7 = dplyr::lead(ClosingPrice.USD., 7),
	btc.lead14 = dplyr::lead(ClosingPrice.USD., 14),
	btc.lead21 = dplyr::lead(ClosingPrice.USD., 21),
	index.lag7 = dplyr::lag(Index, 7),
	index.lead1 = dplyr::lead(Index, 1),
	index.lead7 = dplyr::lead(Index, 7),
	index.lead14 = dplyr::lead(Index, 14),
	index.lead21 = dplyr::lead(Index, 21),
	ret_lastweek_eth = log(Eth) - log(eth.lag7),
	ret_nextweek_eth = log(eth.lead7) - log(Eth),
	ret_nexttwoweek_eth = log(eth.lead14) - log(Eth),
	ret_nextthreeweek_eth = log(eth.lead21) - log(Eth),
	ret_lastweek_index = log(Index) - log(index.lag7),
	ret_nextweek_index = log(index.lead7) - log(Index),
	ret_nexttwoweek_index = log(index.lead14) - log(Index),
	ret_nextthreeweek_index = log(index.lead21) - log(Index),
	ret_lastweek_btc = log(ClosingPrice.USD.) - log(btc.lag7),
	ret_nextweek_btc = log(btc.lead7) - log(ClosingPrice.USD.),
	ret_nexttwoweek_btc = log(btc.lead14) - log(ClosingPrice.USD.),
	ret_nextthreeweek_btc = log(btc.lead21) - log(ClosingPrice.USD.),
	))
df2 <- as.data.frame(df2 %>% mutate(
# Scale all variables
	Z_deaths = scale(total_deaths),
	Z_cases = scale(total_cases),
	Z_new_cases = scale(new_cases),
	Z_new_deaths = scale(new_deaths),
	Z_Prior_10_cases = scale(Prior_10_cases),
	Z_Prior_10_deaths = scale(Prior_10_deaths),
	Z_Prior_7_cases = scale(Prior_7_cases),
	Z_Prior_7_deaths = scale(Prior_7_deaths),
	Z_Prior_14_cases = scale(Prior_14_cases),
	Z_Prior_14_deaths = scale(Prior_14_deaths),
	Z_Prior_10_policy = scale(Prior_10_policy),
	Z_AbCases = scale(AbCases),
	Z_AbDeaths = scale(AbDeaths),
	Z_AbPolicy = scale(AbPolicy),
	Z_ret_eth = scale(ret_t_eth),
	Z_ret_btc = scale(ret_t_btc),
	Z_ret_index = scale(ret_t_index),
	Z_ret_eth_minus_week = scale(ret_lastweek_eth),
	Z_ret_btc_minus_week = scale(ret_lastweek_index),
	Z_ret_index_minus_week = scale(ret_lastweek_btc),
	ret_nextweek_eth = ret_nextweek_eth*100,
	ret_nexttwoweek_eth = ret_nexttwoweek_eth*100,
	ret_nextthreeweek_eth = ret_nextthreeweek_eth*100,
	ret_nextweek_index = ret_nextweek_index*100,
	ret_nexttwoweek_index = ret_nexttwoweek_index*100,
	ret_nextthreeweek_index = ret_nextthreeweek_index*100,
	ret_nextweek_btc = ret_nextweek_btc*100,
	ret_nexttwoweek_btc = ret_nexttwoweek_btc*100,
	ret_nextthreeweek_btc = ret_nextthreeweek_btc*100,
	))
df2 <- df2[complete.cases(df2),]
# Remove return outliers
df2$ret_nextweek_eth <- Winsorize(df2$ret_nextweek_eth)
df2$ret_nextweek_btc <- Winsorize(df2$ret_nextweek_btc)
df2$ret_nextweek_index <- Winsorize(df2$ret_nextweek_index)
df2$ret_nexttwoweek_eth <- Winsorize(df2$ret_nexttwoweek_eth)
df2$ret_nexttwoweek_btc <- Winsorize(df2$ret_nexttwoweek_btc)
df2$ret_nexttwoweek_index <- Winsorize(df2$ret_nexttwoweek_index)
df2$ret_nextthreeweek_eth <- Winsorize(df2$ret_nextthreeweek_eth)
df2$ret_nextthreeweek_btc <- Winsorize(df2$ret_nextthreeweek_btc)
df2$ret_nextthreeweek_index <- Winsorize(df2$ret_nextthreeweek_index)
library(lubridate)
# Create robustness variable
G$Week <- rownames(G)
G$week_num <- week(as.Date(G$Week, "%Y-%m-%d"))
G$year_num <- year(as.Date(G$Week, "%Y-%m-%d"))
G<-G[-1,]
df2$week_num <- week(as.Date(df2$Date, "%Y-%m-%d"))
df2$year_num <- year(df2$Date)
dfxx <- inner_join(df2, G, by=c("week_num", "year_num"))
dfxx$searches <- as.numeric(as.character(dfxx$Category..All.categories))
dfxx <- as.data.frame(dfxx %>% mutate(
	new_search_long = rollapply(lag(searches, 6), 25, mean, fill=NA, alight='right'),
	new_search_short = rollapply(lag(searches, 1), 5, mean, fill=NA, alight='right'),
	AbSearch = new_search_short - new_search_long))
dfxx$AbSearch_Z <- scale(dfxx$AbSearch)
dfxx <- dfxx[complete.cases(dfxx),]
dfxx$Z_AbPolicy <- Winsorize(dfxx$Z_AbPolicy)

postscript("mot.eps")
plot(df2$Date, scale(df2$ClosingPrice.USD.), type='l', 
	ylim=c(-1.5, 2.2),
	xlab="Date", 
	ylab="Z scores")
lines(df2$Date,df2$Z_Prior_7_cases, lty=2)
dev.off()

summary(df2$Prior_10_cases)
summary(df2$Prior_10_deaths)
summary(df2$AbCases)
summary(df2$AbDeaths)

postscript("scale_prior.eps")
par(mfrow=c(1,2))
plot(df2$Date, df2$Z_Prior_7_cases, type='l', xlab="Date", 
	ylab="Sum of cases in last 7 days")
plot(df2$Date, df2$Z_Prior_7_deaths, type='l', xlab="Date",
	ylab="Sum of deaths in last 7 days")
dev.off()

# postscript("scale_ab.eps")
# par(mfrow=c(1,2))
# plot(df2$Date, scale(df2$Z_AbCases), type='l', xlab="Date", 
	# ylab="Abnormal cases in the last week")
# plot(df2$Date, scale(df2$Z_AbDeaths), type='l', xlab="Date",
	# ylab="Abnormal deaths in the last week")
# dev.off()

postscript("policy.eps")
plot(dfxx$Date, dfxx$Z_AbPolicy, type='l', xlab="Date", 
	ylab="Global Covid-19 Policy Stringency")
dev.off()
postscript("invatt.eps")
plot(dfxx$Date, dfxx$AbSearch_Z, type='l', xlab="Date",
	ylab="Investor Attention")
dev.off()

######### baseline regressions - flight to quality########
#cases/deaths and policy 

fit <- lm(ret_nextweek_eth ~  Z_ret_eth_minus_week+Z_Prior_7_cases*Z_AbPolicy, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
high_policy <- mean(dfxx$Z_AbPolicy) + sd(dfxx$Z_AbPolicy)
medium_policy <- mean(dfxx$Z_AbPolicy)
low_policy <- mean(dfxx$Z_AbPolicy) - sd(dfxx$Z_AbPolicy)
high_policy <- round(high_policy, 1)
medium_policy <- round(medium_policy, 1)
low_policy <- round(low_policy, 1)
high_cases <- round(mean(dfxx$Z_Prior_7_cases)+sd(dfxx$Z_Prior_7_cases), 1)
medium_cases <- round(mean(dfxx$Z_Prior_7_cases), 1)
low_cases <- round(mean(dfxx$Z_Prior_7_cases)-sd(dfxx$Z_Prior_7_cases), 1)
effect1 <- effect("Z_Prior_7_cases*Z_AbPolicy", fit,
	xlevels=list(Z_AbPolicy=c(low_policy, medium_policy, high_policy), 
		Z_Prior_7_cases=c(low_cases, medium_cases, high_cases)))
eff1 <- as.data.frame(effect1)
eff1$Z_AbPolicy <- factor(eff1$Z_AbPolicy, 
					levels=c(low_policy, medium_policy, high_policy),
					labels=c("1 SD below",
							"Mean policy intervention",
							"1 SD above"))
eff1$Z_Prior_7_cases <- factor(eff1$Z_Prior_7_cases, 
					levels=c(low_cases, medium_cases, high_cases),
					labels=c("1 SD below",
							"Mean 7 day sum cases",
							"1 SD above"))
library(ggplot2)                
Plot.fit1<-ggplot(data=eff1, 
				aes(x=Z_Prior_7_cases, y=fit, group=Z_AbPolicy))+ 
		geom_point() + geom_line(size=1) + 
		geom_ribbon(aes(ymin=fit-se, ymax=fit+se),alpha=0.3)+labs(title = "", x= "Low, Medium and High Covid cases", 
		y="Ethereum returns (%)", 
		color="Policy Intervention", 
		fill="Policy Intervention") + theme_set(theme_bw()) + theme(legend.position = "none")
Plot.fit1 

fit <- lm(ret_nextweek_eth ~  Z_ret_eth_minus_week+Z_Prior_7_deaths*Z_AbPolicy, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
high_policy <- mean(dfxx$Z_AbPolicy) + sd(dfxx$Z_AbPolicy)
medium_policy <- mean(dfxx$Z_AbPolicy)
low_policy <- mean(dfxx$Z_AbPolicy) - sd(dfxx$Z_AbPolicy)
high_policy <- round(high_policy, 1)
medium_policy <- round(medium_policy, 1)
low_policy <- round(low_policy, 1)
high_deaths <- round(mean(dfxx$Z_Prior_7_deaths)+sd(dfxx$Z_Prior_7_deaths), 1)
medium_deaths <- round(mean(dfxx$Z_Prior_7_deaths), 1)
low_deaths <- round(mean(dfxx$Z_Prior_7_deaths)-sd(dfxx$Z_Prior_7_deaths), 1)
effect1 <- effect("Z_Prior_7_deaths*Z_AbPolicy", fit,
	xlevels=list(Z_AbPolicy=c(low_policy, medium_policy, high_policy), 
		Z_Prior_7_deaths=c(low_deaths, medium_deaths, high_deaths)))
eff1 <- as.data.frame(effect1)
eff1$Z_AbPolicy <- factor(eff1$Z_AbPolicy, 
					levels=c(low_policy, medium_policy, high_policy),
					labels=c("1 SD below",
							"Mean policy intervention",
							"1 SD above"))
eff1$Z_Prior_7_deaths <- factor(eff1$Z_Prior_7_deaths, 
					levels=c(low_deaths, medium_deaths, high_deaths),
					labels=c("1 SD below",
							"Mean 7 day sum deaths",
							"1 SD above"))
library(ggplot2)                
Plot.fit2<-ggplot(data=eff1, 
				aes(x=Z_Prior_7_deaths, y=fit, group=Z_AbPolicy))+ 
		geom_point() + geom_line(size=1) + 
		geom_ribbon(aes(ymin=fit-se, ymax=fit+se),alpha=0.3)+labs(title = "", x= "Low, Medium and High Covid deaths", 
		y="Ethereum returns (%)", 
		color="Policy Intervention", 
		fill="Policy Intervention") + theme_set(theme_bw()) + theme(legend.position = "none")
Plot.fit2

grid.arrange(Plot.fit1 , Plot.fit2 , ncol = 2)
g <- ggarrange(Plot.fit1 , Plot.fit2, ncol = 2)
ggsave("eth.png", g, width = 8, height = 4,dpi = 320)






fit <- lm(ret_nextweek_btc ~  Z_ret_btc_minus_week+Z_Prior_7_cases*Z_AbPolicy, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
high_policy <- mean(dfxx$Z_AbPolicy) + sd(dfxx$Z_AbPolicy)
medium_policy <- mean(dfxx$Z_AbPolicy)
low_policy <- mean(dfxx$Z_AbPolicy) - sd(dfxx$Z_AbPolicy)
high_policy <- round(high_policy, 1)
medium_policy <- round(medium_policy, 1)
low_policy <- round(low_policy, 1)
high_cases <- round(mean(dfxx$Z_Prior_7_cases)+sd(dfxx$Z_Prior_7_cases), 1)
medium_cases <- round(mean(dfxx$Z_Prior_7_cases), 1)
low_cases <- round(mean(dfxx$Z_Prior_7_cases)-sd(dfxx$Z_Prior_7_cases), 1)
effect1 <- effect("Z_Prior_7_cases*Z_AbPolicy", fit,
	xlevels=list(Z_AbPolicy=c(low_policy, medium_policy, high_policy), 
		Z_Prior_7_cases=c(low_cases, medium_cases, high_cases)))
eff1 <- as.data.frame(effect1)
eff1$Z_AbPolicy <- factor(eff1$Z_AbPolicy, 
					levels=c(low_policy, medium_policy, high_policy),
					labels=c("1 SD below",
							"Mean policy intervention",
							"1 SD above"))
eff1$Z_Prior_7_cases <- factor(eff1$Z_Prior_7_cases, 
					levels=c(low_cases, medium_cases, high_cases),
					labels=c("1 SD below",
							"Mean 7 day sum cases",
							"1 SD above"))
library(ggplot2)                
Plot.fit1<-ggplot(data=eff1, 
				aes(x=Z_Prior_7_cases, y=fit, group=Z_AbPolicy))+ 
		geom_point() + geom_line(size=1) + 
		geom_ribbon(aes(ymin=fit-se, ymax=fit+se),alpha=0.3)+labs(title = "", x= "Low, Medium and High Covid cases", 
		y="Bitcoin returns (%)", 
		color="Policy Intervention", 
		fill="Policy Intervention") + theme_set(theme_bw()) + theme(legend.position = "none")
Plot.fit1 

fit <- lm(ret_nextweek_btc ~  Z_ret_btc_minus_week+Z_Prior_7_deaths*Z_AbPolicy, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
high_policy <- mean(dfxx$Z_AbPolicy) + sd(dfxx$Z_AbPolicy)
medium_policy <- mean(dfxx$Z_AbPolicy)
low_policy <- mean(dfxx$Z_AbPolicy) - sd(dfxx$Z_AbPolicy)
high_policy <- round(high_policy, 1)
medium_policy <- round(medium_policy, 1)
low_policy <- round(low_policy, 1)
high_deaths <- round(mean(dfxx$Z_Prior_7_deaths)+sd(dfxx$Z_Prior_7_deaths), 1)
medium_deaths <- round(mean(dfxx$Z_Prior_7_deaths), 1)
low_deaths <- round(mean(dfxx$Z_Prior_7_deaths)-sd(dfxx$Z_Prior_7_deaths), 1)
effect1 <- effect("Z_Prior_7_deaths*Z_AbPolicy", fit,
	xlevels=list(Z_AbPolicy=c(low_policy, medium_policy, high_policy), 
		Z_Prior_7_deaths=c(low_deaths, medium_deaths, high_deaths)))
eff1 <- as.data.frame(effect1)
eff1$Z_AbPolicy <- factor(eff1$Z_AbPolicy, 
					levels=c(low_policy, medium_policy, high_policy),
					labels=c("1 SD below",
							"Mean policy intervention",
							"1 SD above"))
eff1$Z_Prior_7_deaths <- factor(eff1$Z_Prior_7_deaths, 
					levels=c(low_deaths, medium_deaths, high_deaths),
					labels=c("1 SD below",
							"Mean 7 day sum deaths",
							"1 SD above"))
library(ggplot2)                
Plot.fit2<-ggplot(data=eff1, 
				aes(x=Z_Prior_7_deaths, y=fit, group=Z_AbPolicy))+ 
		geom_point() + geom_line(size=1) + 
		geom_ribbon(aes(ymin=fit-se, ymax=fit+se),alpha=0.3)+labs(title = "", x= "Low, Medium and High Covid deaths", 
		y="Bitcoin returns (%)", 
		color="Policy Intervention", 
		fill="Policy Intervention") + theme_set(theme_bw()) + theme(legend.position = "none")
Plot.fit2

grid.arrange(Plot.fit1 , Plot.fit2 , ncol = 2)
g <- ggarrange(Plot.fit1 , Plot.fit2, ncol = 2)
ggsave("btc.png", g, width = 8, height = 4,dpi = 320)









fit <- lm(ret_nextweek_index ~  Z_ret_index_minus_week+Z_Prior_7_cases*Z_AbPolicy, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
high_policy <- mean(dfxx$Z_AbPolicy) + sd(dfxx$Z_AbPolicy)
medium_policy <- mean(dfxx$Z_AbPolicy)
low_policy <- mean(dfxx$Z_AbPolicy) - sd(dfxx$Z_AbPolicy)
high_policy <- round(high_policy, 1)
medium_policy <- round(medium_policy, 1)
low_policy <- round(low_policy, 1)
high_cases <- round(mean(dfxx$Z_Prior_7_cases)+sd(dfxx$Z_Prior_7_cases), 1)
medium_cases <- round(mean(dfxx$Z_Prior_7_cases), 1)
low_cases <- round(mean(dfxx$Z_Prior_7_cases)-sd(dfxx$Z_Prior_7_cases), 1)
effect1 <- effect("Z_Prior_7_cases*Z_AbPolicy", fit,
	xlevels=list(Z_AbPolicy=c(low_policy, medium_policy, high_policy), 
		Z_Prior_7_cases=c(low_cases, medium_cases, high_cases)))
eff1 <- as.data.frame(effect1)
eff1$Z_AbPolicy <- factor(eff1$Z_AbPolicy, 
					levels=c(low_policy, medium_policy, high_policy),
					labels=c("1 SD below",
							"Mean policy intervention",
							"1 SD above"))
eff1$Z_Prior_7_cases <- factor(eff1$Z_Prior_7_cases, 
					levels=c(low_cases, medium_cases, high_cases),
					labels=c("1 SD below",
							"Mean 7 day sum cases",
							"1 SD above"))
library(ggplot2)                
Plot.fit1<-ggplot(data=eff1, 
				aes(x=Z_Prior_7_cases, y=fit, group=Z_AbPolicy))+ 
		geom_point() + geom_line(size=1) + 
		geom_ribbon(aes(ymin=fit-se, ymax=fit+se),alpha=0.3)+labs(title = "", x= "Low, Medium and High Covid cases", 
		y="CCi3 returns (%)", 
		color="Policy Intervention", 
		fill="Policy Intervention") + theme_set(theme_bw()) + theme(legend.position = "none")
Plot.fit1 

fit <- lm(ret_nextweek_index ~  Z_ret_index_minus_week+Z_Prior_7_deaths*Z_AbPolicy, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
high_policy <- mean(dfxx$Z_AbPolicy) + sd(dfxx$Z_AbPolicy)
medium_policy <- mean(dfxx$Z_AbPolicy)
low_policy <- mean(dfxx$Z_AbPolicy) - sd(dfxx$Z_AbPolicy)
high_policy <- round(high_policy, 1)
medium_policy <- round(medium_policy, 1)
low_policy <- round(low_policy, 1)
high_deaths <- round(mean(dfxx$Z_Prior_7_deaths)+sd(dfxx$Z_Prior_7_deaths), 1)
medium_deaths <- round(mean(dfxx$Z_Prior_7_deaths), 1)
low_deaths <- round(mean(dfxx$Z_Prior_7_deaths)-sd(dfxx$Z_Prior_7_deaths), 1)
effect1 <- effect("Z_Prior_7_deaths*Z_AbPolicy", fit,
	xlevels=list(Z_AbPolicy=c(low_policy, medium_policy, high_policy), 
		Z_Prior_7_deaths=c(low_deaths, medium_deaths, high_deaths)))
eff1 <- as.data.frame(effect1)
eff1$Z_AbPolicy <- factor(eff1$Z_AbPolicy, 
					levels=c(low_policy, medium_policy, high_policy),
					labels=c("1 SD below",
							"Mean policy intervention",
							"1 SD above"))
eff1$Z_Prior_7_deaths <- factor(eff1$Z_Prior_7_deaths, 
					levels=c(low_deaths, medium_deaths, high_deaths),
					labels=c("1 SD below",
							"Mean 7 day sum deaths",
							"1 SD above"))
library(ggplot2)                
Plot.fit2<-ggplot(data=eff1, 
				aes(x=Z_Prior_7_deaths, y=fit, group=Z_AbPolicy))+ 
		geom_point() + geom_line(size=1) + 
		geom_ribbon(aes(ymin=fit-se, ymax=fit+se),alpha=0.3)+labs(title = "", x= "Low, Medium and High Covid deaths", 
		y="CCi3 returns (%)", 
		color="Policy Intervention", 
		fill="Policy Intervention") + theme_set(theme_bw()) + theme(legend.position = "none")
Plot.fit2

grid.arrange(Plot.fit1 , Plot.fit2 , ncol = 2)
g <- ggarrange(Plot.fit1 , Plot.fit2, ncol = 2)
ggsave("index.png", g, width = 8, height = 4,dpi = 320)



# BENCHMARK REPEATED



fit <- lm(ret_nextweek_eth ~  Z_ret_eth_minus_week+Z_Prior_7_cases*Z_AbPolicy, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_eth ~  Z_ret_eth_minus_week+Z_Prior_7_deaths*Z_AbPolicy, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_btc ~  Z_ret_btc_minus_week+Z_Prior_7_cases*Z_AbPolicy, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_btc ~  Z_ret_btc_minus_week+Z_Prior_7_deaths*Z_AbPolicy, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_index ~  Z_ret_index_minus_week+Z_Prior_7_cases*Z_AbPolicy, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_index ~  Z_ret_index_minus_week+Z_Prior_7_deaths*Z_AbPolicy, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))





# ROBUSTNESS

# PLUS SEARCHES

fit <- lm(ret_nextweek_eth ~  Z_ret_eth_minus_week+Z_Prior_7_cases*Z_AbPolicy +AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_btc ~  Z_ret_btc_minus_week+Z_Prior_7_cases*Z_AbPolicy +AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_index ~  Z_ret_index_minus_week+Z_Prior_7_cases*Z_AbPolicy +AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))

fit <- lm(ret_nextweek_eth ~  Z_ret_eth_minus_week+Z_Prior_7_deaths*Z_AbPolicy +AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_btc ~  Z_ret_btc_minus_week+Z_Prior_7_deaths*Z_AbPolicy +AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_index ~  Z_ret_index_minus_week+Z_Prior_7_deaths*Z_AbPolicy +AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))

# Different covid/death periods

fit <- lm(ret_nextweek_eth ~  Z_ret_eth_minus_week+Z_Prior_7_cases*Z_AbPolicy , data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_btc ~  Z_ret_btc_minus_week+Z_Prior_7_cases*Z_AbPolicy , data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_index ~  Z_ret_index_minus_week+Z_Prior_7_cases*Z_AbPolicy , data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))


fit <- lm(ret_nextweek_eth ~  Z_ret_eth_minus_week+Z_Prior_14_cases*Z_AbPolicy , data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_btc ~  Z_ret_btc_minus_week+Z_Prior_14_cases*Z_AbPolicy , data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_index ~  Z_ret_index_minus_week+Z_Prior_14_cases*Z_AbPolicy , data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))


# Different forward looking periods

fit <- lm(ret_nexttwoweek_eth ~  Z_ret_eth_minus_week+Z_Prior_10_cases*Z_AbPolicy +AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nexttwoweek_btc ~  Z_ret_btc_minus_week+Z_Prior_10_cases*Z_AbPolicy +AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nexttwoweek_index ~  Z_ret_index_minus_week+Z_Prior_10_cases*Z_AbPolicy +AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))


fit <- lm(ret_nextthreeweek_eth ~  Z_ret_eth_minus_week+Z_Prior_10_cases*Z_AbPolicy +AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextthreeweek_btc ~  Z_ret_btc_minus_week+Z_Prior_10_cases*Z_AbPolicy +AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextthreeweek_index ~  Z_ret_index_minus_week+Z_Prior_10_cases*Z_AbPolicy +AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))



fit <- lm(ret_nextweek_eth ~  Z_ret_eth_minus_week+Z_Prior_10_deaths*Z_AbPolicy +AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_btc ~  Z_ret_btc_minus_week+Z_Prior_10_deaths*Z_AbPolicy +AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_index ~  Z_ret_index_minus_week+Z_Prior_10_deaths*Z_AbPolicy +AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))


fit <- lm(ret_nexttwoweek_eth ~  Z_ret_eth_minus_week+Z_Prior_10_deaths*Z_AbPolicy +AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nexttwoweek_btc ~  Z_ret_btc_minus_week+Z_Prior_10_deaths*Z_AbPolicy +AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nexttwoweek_index ~  Z_ret_index_minus_week+Z_Prior_10_deaths*Z_AbPolicy +AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))


fit <- lm(ret_nextthreeweek_eth ~  Z_ret_eth_minus_week+Z_Prior_10_deaths*Z_AbPolicy +AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextthreeweek_btc ~  Z_ret_btc_minus_week+Z_Prior_10_deaths*Z_AbPolicy +AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextthreeweek_index ~  Z_ret_index_minus_week+Z_Prior_10_deaths*Z_AbPolicy +AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
