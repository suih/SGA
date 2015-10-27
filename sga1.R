# SPEND VS. GROSS ADDS Relationship Exploration
# OCT,2015
rm(list=ls())
require(RPresto)
require(astsa)
require(vars)
require(plyr)
ConnectNFLXPresto = function(username) {
  con <- dbConnect(
    RPresto::Presto(),
    host='http://proxy.dataeng.netflix.net',
    port=8080,
    user=username,
    schema='default',
    catalog='hive'
  )
  return (con)
}
con=ConnectNFLXPresto('shuang')
source('~/scicomp_utils/R/RPrestoClients/R_BigData.R')
sga_region=dbGetQuery(con,"select *,(signups-signups_fraud)as signup_adj from mendyh.sga_data_region")
sga_region$dollar_efficiency<-sga_region$signup_adj/sga_region$bbhh_period_end/sga_region$total_media_spend_rebate_adj
sga_region<-sga_region[with(sga_region,order(region_desc,period_nbr)),]
sga_region$pen<-sga_region$membership_period_end/sga_region$bbhh_period_end
sga_region$signup_bbhh<-sga_region$signup_adj/sga_region$bbhh_period_end
require(ggplot2)
head(sga_region)
ggplot(data=sga_region,aes(x=factor(period_nbr),y=dollar_efficiency,color=region_desc,group=region_desc))+geom_line()+theme_bw()+ggtitle('Descriptive Dollar Efficiency')
ggplot(data=sga_region,aes(x=factor(period_nbr),y=pen,color=region_desc,group=region_desc))+geom_line()+theme_bw()+ggtitle('Descriptive Penetration Trend')
ggplot(data=sga_region,aes(x=factor(period_nbr),y=signup_bbhh,color=region_desc,group=region_desc))+geom_line()+theme_bw()+ggtitle('Descriptive Relative Gross Adds Overtime')
us<-subset(sga_region,region_desc=='United States')
us<-us[with(us,order(period_nbr)),]
a<-ts(us$signup_bbhh)
plot(a,type='b')
adiff<-diff(a,12)
acf2(adiff,48)
model1<-sarima(a, 1,0,0,0,1,1,12)


# READ QUARTERLY DATA FROM CSV
library(zoo)
ts1 <- read.zoo('Documents/data/macros.csv', header = T, sep = ",", FUN = as.yearqtr)

# CONVERT THE DATA TO STATIONARY TIME SERIES
ts1$hpi_rate <- log(ts1$hpi / lag(ts1$hpi))
ts1$unemp_rate <- log(ts1$unemp / lag(ts1$unemp))
ts2 <- ts1[1:nrow(ts1) - 1, c(3, 4)]

# METHOD 1: LMTEST PACKAGE
library(lmtest)
grangertest(unemp_rate ~ hpi_rate, order = 1, data = ts2)
# Granger causality test
#
# Model 1: unemp_rate ~ Lags(unemp_rate, 1:1) + Lags(hpi_rate, 1:1)
# Model 2: unemp_rate ~ Lags(unemp_rate, 1:1)
#   Res.Df Df      F  Pr(>F)
# 1     55
# 2     56 -1 4.5419 0.03756 *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# METHOD 2: VARS PACKAGE
library(vars)
var <- VAR(ts2, p = 1, type = "const")
causality(var, cause = "hpi_rate")$Granger
#         Granger causality H0: hpi_rate do not Granger-cause unemp_rate
#
# data:  VAR object var
# F-Test = 4.5419, df1 = 1, df2 = 110, p-value = 0.0353

# AUTOMATICALLY SEARCH FOR THE MOST SIGNIFICANT RESULT
for (i in 1:4)
{
  cat("LAG =", i)
  print(causality(VAR(ts2, p = i, type = "const"), cause = "hpi_rate")$Granger)
}

