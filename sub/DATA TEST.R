library("readxl")
library(tidyverse)
library(gghighlight)
library(tibbletime)
library(dplyr)
library(rlang)
library(furrr)
library(momentfit)
library(tseries)

data <- read_csv("/Users/jinkim/Library/CloudStorage/OneDrive-Personal/Aucklanduni/2023 S2/ECON 723/Group Project/data_nz_filled.csv")



swap.ts = ts(data$Swap_rates_1year,frequency=12,start = c(2010, 8))
swap.ts

plot(data$Swap_rates_1year,main="Swap Rate for 1 Year Period",xlab="month",ylab="swap rate in percent",frequency=12,start = c(2010, 8))
plot(data$bond_closing_yields_1year,main="Yields Rate for 1 Year Period",xlab="month",ylab="yields rate in percent",frequency=12,start = c(2010, 8))


plot(data$Swap_rates_2year,main="Swap Rate for 2 Year Period",xlab="month",ylab="swap rate in percent",frequency=12,start = c(2010, 8))
plot(data$bond_closing_yields_2year,main="Yields Rate for 2 Year Period",xlab="month",ylab="yields rate in percent",frequency=12,start = c(2010, 8))


fit <- lm(data$bond_closing_yields_1year~data$Swap_rates_1year)
summary(fit)

fit <- lm(data$bond_closing_yields_2year~data$Swap_rates_2year)
summary(fit)


plot(data$Swap_rates_5year)
plot(data$Swap_rates_10year)

plot(data$bond_closing_yields_1year)
plot(data$bond_closing_yields_2year)
plot(data$bond_closing_yields_5year)
plot(data$bond_closing_yields_10year)

plot.ts(swap.ts,main="Swap Rate for 1 Year Period",xlab="month",ylab="swap rate in percent",frequency=12,start = c(2010, 8))
acf(swap.ts,lag.max=48)  #autocorrelation
acf((swap.ts-mean(swap.ts))^2 ,lag.max=48) #Heteroscedasticity 
adf.test(swap.ts) #stationary test, Time series are stationary if they do not have trend or seasonal effects, p-value is obtained is greater than significance level of 0.05 and the ADF statistic is higher than any of the critical values. Clearly, there is no reason to reject the null hypothesis. So, the time series is in fact non-stationary.

decomp.swap = decompose(swap.ts)
plot(decomp.swap)

#decomp.swap = stl(swap.ts,s.window="periodic")
#decomp.swap
#plot(decomp.swap,main="swap rate")




swap.ts1 = ts(data$Swap_rates_2year,frequency=12,start = c(2010, 8))
swap.ts1
plot.ts(swap.ts1,main="Swap Rate for 2 Year Period",xlab="month",ylab="swap rate in percent",frequency=12,start = c(2010, 8))
acf(swap.ts1,lag.max=48)  #autocorrelation
acf((swap.ts1-mean(swap.ts1))^2) #Heteroscedasticity 
adf.test(swap.ts1) #stationary test, Time series are stationary if they do not have trend or seasonal effects, p-value is obtained is greater than significance level of 0.05 and the ADF statistic is higher than any of the critical values. Clearly, there is no reason to reject the null hypothesis. So, the time series is in fact non-stationary.

decomp.swap1 = decompose(swap.ts1)
plot(decomp.swap1)


Yield.ts = ts(data$bond_closing_yields_1year,frequency=12,start = c(2010, 8))
Yield.ts
plot.ts(Yield.ts,main="Yield Rate for 1 Year Period",xlab="month",ylab="yield rate in percent",frequency=12,start = c(2010, 8))
acf(Yield.ts,lag.max=48)  #autocorrelation
acf((Yield.ts-mean(Yield.ts))^2) #Heteroscedasticity 
adf.test(Yield.ts) #stationary test, Time series are stationary if they do not have trend or seasonal effects, p-value is obtained is greater than significance level of 0.05 and the ADF statistic is higher than any of the critical values. Clearly, there is no reason to reject the null hypothesis. So, the time series is in fact non-stationary.

decomp.yield = decompose(Yield.ts)
plot(decomp.yield)

Yield.ts1 = ts(data$bond_closing_yields_2year,frequency=12,start = c(2010, 8))
Yield.ts1
plot.ts(Yield.ts1,main="Yield Rate for 2 Year Period",xlab="month",ylab="yield rate in percent",frequency=12,start = c(2010, 8))
acf(Yield.ts1,lag.max=48)  #autocorrelation
acf((Yield.ts1-mean(Yield.ts1))^2) #Heteroscedasticity 
adf.test(Yield.ts1) #stationary test, Time series are stationary if they do not have trend or seasonal effects, p-value is obtained is greater than significance level of 0.05 and the ADF statistic is higher than any of the critical values. Clearly, there is no reason to reject the null hypothesis. So, the time series is in fact non-stationary.

decomp.yield1 = decompose(Yield.ts1)
plot(decomp.yield1)



data_inter <- read_csv("/Users/jinkim/Library/CloudStorage/OneDrive-Personal/Aucklanduni/2023 S2/ECON 723/Group Project/nz.csv")
interbank.ts = ts(data_inter$interbank,frequency=12,start = c(2003, 7))
interbank.ts
plot.ts(interbank.ts,main="Interbank Rate for 1 Year Period",xlab="month",ylab="interbank rate in percent",frequency=12,start = c(2003, 7))
acf(interbank.ts,lag.max=48)  #autocorrelation
acf((interbank.ts-mean(interbank.ts))^2 ,lag.max=48) #Heteroscedasticity 
adf.test(interbank.ts) #stationary test, Time series are stationary if they do not have trend or seasonal effects, p-value is obtained is greater than significance level of 0.05 and the ADF statistic is higher than any of the critical values. Clearly, there is no reason to reject the null hypothesis. So, the time series is in fact non-stationary.

decomp.interbank = decompose(interbank.ts)
plot(decomp.interbank)

plot(data_inter$interbank,main="Interbank Rate for 1 Year Period",xlab="month",ylab="swap rate in percent",frequency=12,start = c(2003, 7))
plot(data_inter$bond_closing_yields_1year,main="Yields Rate for 1 Year Period",xlab="month",ylab="yields rate in percent",frequency=12,start = c(2003, 7))







bond_closing_yields_1year.ts = ts(data_inter$bond_closing_yields_1year,frequency=12,start = c(2003, 7))
bond_closing_yields_1year.ts
plot.ts(bond_closing_yields_1year.ts,main="Yield Rate for 1 Year Period",xlab="month",ylab="yield rate in percent",frequency=12,start = c(2003, 7))
acf(bond_closing_yields_1year.ts,lag.max=48)  #autocorrelation
acf((bond_closing_yields_1year.ts-mean(bond_closing_yields_1year.ts))^2 ,lag.max=48) #Heteroscedasticity 
adf.test(bond_closing_yields_1year.ts) #stationary test, Time series are stationary if they do not have trend or seasonal effects, p-value is obtained is greater than significance level of 0.05 and the ADF statistic is higher than any of the critical values. Clearly, there is no reason to reject the null hypothesis. So, the time series is in fact non-stationary.

decomp.bond_closing_yields_1year = decompose(bond_closing_yields_1year.ts)
plot(decomp.bond_closing_yields_1year)








data_f <- read_csv("/Users/jinkim/Library/CloudStorage/OneDrive-Personal/Aucklanduni/2023 S2/ECON 723/Group Project/final data.csv")
y1.ts = ts(data_f$r1y_nz,frequency=12,start = c(1987, 7))
y1.ts
plot.ts(y1.ts,main="Yields Rate for 1 Year Period",xlab="Year",ylab="yield rate in percent",frequency=12,start = c(1987, 7))

y2.ts = ts(data_f$r2y_nz,frequency=12,start = c(1987, 7))
y2.ts
plot.ts(y2.ts,main="Yields Rate for 2 Year Period",xlab="Year",ylab="yield rate in percent",frequency=12,start = c(1987, 7))



plot(data_f$r1y_nz,main="Yields Rate for 1 Year Period",xlab="month",ylab="yield rate in percent",frequency=12,start = c(1987, 7))
plot(data_f$r2y_gb,main="Yields Rate for 2 Year Period",xlab="month",ylab="yield rate in percent",frequency=12,start = c(1987, 7))

