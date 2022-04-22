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

# last three weeks mean, lagged by a week. First 3 weeks mean in a month, given at the end of the month
new_policy_long = lag(rollapply(mean_policy_response, 28, mean, fill=NA, align='right'),8),
# Last week lagged by a day
new_policy_short = lag(rollapply(mean_policy_response, 7, mean, fill=NA, align='right')),
AbPolicy = new_policy_short - new_policy_long,

# Last week return, lagged by a day
Last_Week_Eth_Ret = lag(rollapply(Eth, width=7, RetList , fill=NA, align='right')),
Last_Week_Btc_Ret = lag(rollapply(ClosingPrice.USD., width=7, RetList, fill=NA, align='right')),
Last_Week_Index_Ret = lag(rollapply(Index, width=7, RetList, fill=NA, align='right')),

# Forward looking weekly return, above variable reordered in time
Next_Week_Eth_Ret = lead(Last_Week_Eth_Ret, 8),
Next_Week_Btc_Ret = lead(Last_Week_Btc_Ret, 8),
Next_Week_Index_Ret = lead(Last_Week_Index_Ret, 8),	

))


df2 <- as.data.frame(df2 %>% mutate(
# Scale all variables
	Z_Prior_7_cases = scale(Prior_7_cases),
	Z_Prior_7_deaths = scale(Prior_7_deaths),
	Z_AbPolicy = scale(AbPolicy),
	Z_ret_eth = scale(Last_Week_Eth_Ret),
	Z_ret_btc = scale(Last_Week_Btc_Ret),
	Z_ret_index = scale(Last_Week_Index_Ret),
	ret_nextweek_eth = Next_Week_Eth_Ret*100,
	ret_nextweek_btc = Next_Week_Btc_Ret*100,
	ret_nextweek_index = Next_Week_Index_Ret*100,
	))
	
df2 <- df2[complete.cases(df2),]

df2 <- dplyr::filter(df2, Date<"2021-11-26")

fit <- lm(ret_nextweek_eth ~ Z_Prior_7_cases*Z_AbPolicy + Z_Prior_7_cases + Z_AbPolicy + Z_ret_eth, data=df2)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))

fit <- lm(ret_nextweek_btc ~ Z_Prior_7_cases*Z_AbPolicy + Z_Prior_7_cases + Z_AbPolicy + Z_ret_btc, data=df2)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))

fit <- lm(ret_nextweek_index ~ Z_Prior_7_cases*Z_AbPolicy + Z_Prior_7_cases + Z_AbPolicy + Z_ret_index, data=df2)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))






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

fit <- lm(ret_nextweek_eth ~ Z_Prior_7_cases*Z_AbPolicy + Z_Prior_7_cases + Z_AbPolicy + Z_ret_eth, data=dfxx)
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

fit <- lm(ret_nextweek_eth ~ Z_Prior_7_deaths*Z_AbPolicy + Z_Prior_7_deaths + Z_AbPolicy + Z_ret_eth, data=dfxx)
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






fit <- lm(ret_nextweek_btc ~ Z_Prior_7_cases*Z_AbPolicy + Z_Prior_7_cases + Z_AbPolicy + Z_ret_btc, data=dfxx)
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

fit <- lm(ret_nextweek_btc ~ Z_Prior_7_deaths*Z_AbPolicy + Z_Prior_7_deaths + Z_AbPolicy + Z_ret_btc, data=dfxx)
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









fit <- lm(ret_nextweek_index ~ Z_Prior_7_cases*Z_AbPolicy + Z_Prior_7_cases + Z_AbPolicy + Z_ret_index, data=dfxx)
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

fit <- lm(ret_nextweek_index ~ Z_Prior_7_deaths*Z_AbPolicy + Z_Prior_7_deaths + Z_AbPolicy + Z_ret_index, data=dfxx)
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

fit <- lm(ret_nextweek_eth ~ Z_Prior_7_cases*Z_AbPolicy + Z_Prior_7_cases + Z_AbPolicy + Z_ret_eth, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_eth ~ Z_Prior_7_deaths*Z_AbPolicy + Z_Prior_7_deaths + Z_AbPolicy + Z_ret_eth, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_btc ~ Z_Prior_7_cases*Z_AbPolicy + Z_Prior_7_cases + Z_AbPolicy + Z_ret_btc, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_btc ~ Z_Prior_7_deaths*Z_AbPolicy + Z_Prior_7_deaths + Z_AbPolicy + Z_ret_btc, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_index ~ Z_Prior_7_cases*Z_AbPolicy + Z_Prior_7_cases + Z_AbPolicy + Z_ret_index, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_index ~ Z_Prior_7_deaths*Z_AbPolicy + Z_Prior_7_deaths + Z_AbPolicy + Z_ret_index, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))

# ROBUSTNESS

# PLUS SEARCHES

fit <- lm(ret_nextweek_eth ~ Z_Prior_7_cases*Z_AbPolicy + Z_Prior_7_cases + Z_AbPolicy + Z_ret_eth + AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_eth ~ Z_Prior_7_deaths*Z_AbPolicy + Z_Prior_7_deaths + Z_AbPolicy + Z_ret_eth + AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_btc ~ Z_Prior_7_cases*Z_AbPolicy + Z_Prior_7_cases + Z_AbPolicy + Z_ret_btc + AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_btc ~ Z_Prior_7_deaths*Z_AbPolicy + Z_Prior_7_deaths + Z_AbPolicy + Z_ret_btc + AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_index ~ Z_Prior_7_cases*Z_AbPolicy + Z_Prior_7_cases + Z_AbPolicy + Z_ret_index + AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
fit <- lm(ret_nextweek_index ~ Z_Prior_7_deaths*Z_AbPolicy + Z_Prior_7_deaths + Z_AbPolicy + Z_ret_index + AbSearch_Z, data=dfxx)
coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))

# # Different covid/death periods

# fit <- lm(ret_nextweek_eth ~  Z_ret_eth_minus_week+Z_Prior_7_cases*Z_AbPolicy , data=dfxx)
# coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
# fit <- lm(ret_nextweek_btc ~  Z_ret_btc_minus_week+Z_Prior_7_cases*Z_AbPolicy , data=dfxx)
# coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
# fit <- lm(ret_nextweek_index ~  Z_ret_index_minus_week+Z_Prior_7_cases*Z_AbPolicy , data=dfxx)
# coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))


# fit <- lm(ret_nextweek_eth ~  Z_ret_eth_minus_week+Z_Prior_14_cases*Z_AbPolicy , data=dfxx)
# coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
# fit <- lm(ret_nextweek_btc ~  Z_ret_btc_minus_week+Z_Prior_14_cases*Z_AbPolicy , data=dfxx)
# coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
# fit <- lm(ret_nextweek_index ~  Z_ret_index_minus_week+Z_Prior_14_cases*Z_AbPolicy , data=dfxx)
# coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))


# # Different forward looking periods

# fit <- lm(ret_nexttwoweek_eth ~  Z_ret_eth_minus_week+Z_Prior_10_cases*Z_AbPolicy +AbSearch_Z, data=dfxx)
# coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
# fit <- lm(ret_nexttwoweek_btc ~  Z_ret_btc_minus_week+Z_Prior_10_cases*Z_AbPolicy +AbSearch_Z, data=dfxx)
# coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
# fit <- lm(ret_nexttwoweek_index ~  Z_ret_index_minus_week+Z_Prior_10_cases*Z_AbPolicy +AbSearch_Z, data=dfxx)
# coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))


# fit <- lm(ret_nextthreeweek_eth ~  Z_ret_eth_minus_week+Z_Prior_10_cases*Z_AbPolicy +AbSearch_Z, data=dfxx)
# coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
# fit <- lm(ret_nextthreeweek_btc ~  Z_ret_btc_minus_week+Z_Prior_10_cases*Z_AbPolicy +AbSearch_Z, data=dfxx)
# coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
# fit <- lm(ret_nextthreeweek_index ~  Z_ret_index_minus_week+Z_Prior_10_cases*Z_AbPolicy +AbSearch_Z, data=dfxx)
# coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))



# fit <- lm(ret_nextweek_eth ~  Z_ret_eth_minus_week+Z_Prior_10_deaths*Z_AbPolicy +AbSearch_Z, data=dfxx)
# coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
# fit <- lm(ret_nextweek_btc ~  Z_ret_btc_minus_week+Z_Prior_10_deaths*Z_AbPolicy +AbSearch_Z, data=dfxx)
# coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
# fit <- lm(ret_nextweek_index ~  Z_ret_index_minus_week+Z_Prior_10_deaths*Z_AbPolicy +AbSearch_Z, data=dfxx)
# coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))


# fit <- lm(ret_nexttwoweek_eth ~  Z_ret_eth_minus_week+Z_Prior_10_deaths*Z_AbPolicy +AbSearch_Z, data=dfxx)
# coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
# fit <- lm(ret_nexttwoweek_btc ~  Z_ret_btc_minus_week+Z_Prior_10_deaths*Z_AbPolicy +AbSearch_Z, data=dfxx)
# coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
# fit <- lm(ret_nexttwoweek_index ~  Z_ret_index_minus_week+Z_Prior_10_deaths*Z_AbPolicy +AbSearch_Z, data=dfxx)
# coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))


# fit <- lm(ret_nextthreeweek_eth ~  Z_ret_eth_minus_week+Z_Prior_10_deaths*Z_AbPolicy +AbSearch_Z, data=dfxx)
# coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
# fit <- lm(ret_nextthreeweek_btc ~  Z_ret_btc_minus_week+Z_Prior_10_deaths*Z_AbPolicy +AbSearch_Z, data=dfxx)
# coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
# fit <- lm(ret_nextthreeweek_index ~  Z_ret_index_minus_week+Z_Prior_10_deaths*Z_AbPolicy +AbSearch_Z, data=dfxx)
# coeftest(fit, vcov = NeweyWest(fit, lag = 7, prewhite = FALSE))
