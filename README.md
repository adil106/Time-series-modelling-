# Time-series-modelling-
Describes the application of various time series models in finance- risk measurement, forecasting and management
## Adil Shah
#Priliminary measures &
#ARIMA- forecast, multivariate ts-relationship, cause and effect, relationship, spillover import excel file
attach(rrmul)
is.na(rrmul)
x=na.omit(rrmul)
View(x)
attach(x)
plot.ts(Exc)
plot.ts(Brent)
plot.ts(Gold)
plot.ts(Sp)
install.packages("tseries")
library(tseries)
adf.test(Exc)
pp.test(Exc)
kpss.test(Exc)
#likwise, perform unit root test to other variables
adf.test(Brent)
pp.test(Brent)
kpss.test(Brent)
#to convert non-staionary stochastic times series into stationarity
rprice=diff(log(Price))
plot.ts(Brent,col="blue")
adf.test(rprice)
pp.test(rprice)
#to remove exponential terms-after point small terms
options(scipen = 999)
#Descriptive stat
install.packages("fBasics")
library(fBasics)
basicStats(data.frame(Exc, Brent, Gold, Sp))
Y= basicStats(data.frame(Exc, Brent, Gold, Sp))
#Descriptive stat stored in docuements with file name Y.csv
write.csv(Y, file = "Y.csv", row.names = FALSE)

#ARIMA-
#how to know the optimal lag
install.packages("forecast")
library(forecast)
auto.arima(Exc)
auto.arima(Brent)
auto.arima(Gold)
auto.arima(Sp)
#to create a forecasting model- out sample and in sample model
#In sample model-splitting a model-such as leaving last ten observations 
model1 <- arima(Sp[1:3500], order = c(0, 0, 2))
model2 <- arima(Gold[1:3500], order = c(0, 0, 0))
model3 <- arima(Brent[1:3500], order = c(0, 0, 0))
model4 <- arima(Exc[1:3500], order = c(0, 0, 1))

library(forecast)
forecast_result <- forecast(model1, h = 24)
plot.ts(forecast_result)
print(forecast_result)
#comparing actual and forecasted results
plot(Sp, col = "yellow", type = "l", lty = 1, ylim = range(c(rprice, forecast_result$mean)))
lines(forecast_result$mean, col = "red", type = "l", lty = 2)
legend("topright", legend = c("Actual", "Forecast"), col = c("yellow", "red"), lty = 1:2)
tail(Sp)
##forecast for other variables
forecast_result2 <- forecast(model2, h = 24)
plot.ts(forecast_result2)
print(forecast_result2)
#comparing actual and forecasted results
plot(Gold, col = "yellow", type = "l", lty = 1, ylim = range(c(Gold, forecast_result$mean)))
lines(forecast_result2$mean, col = "red", type = "l", lty = 2)
legend("topright", legend = c("Actual", "Forecast"), col = c("yellow", "red"), lty = 1:2)
tail(Gold)
#to get minimum sum of squares for model accuracy
accuracy(model1)
coef(model1)
accuracy(model2)
coef(model2)
#Diagonistic-testss
#normality of residuals
et=residuals(model1)
plot.ts(et)
plot(fitted(model1), et, main = "et vs Fitted Values", xlab = "Fitted Values", ylab = "et")
gghistogram(et)
hist(et, main = "Histogram of et", xlab = "et")
#signifiance for normality-Shapiro-Wilk test for normality- The data follows a normal distribution, p less than 0.05- data not normal
shapiro.test(et)
#are residuals correlated
acf(et)
#or
acf(et, main = "ACF of et")
#significance test for ACF- Ljung-box- null hypothesis of no autocorrelation-i.i.d
acf_test <- Box.test(et, type   = "Ljung-Box")
print(acf_test)
##likewise for other variables
et1=residuals(model2)
plot.ts(et1)
plot(fitted(model2), et1, main = "et1 vs Fitted Values", xlab = "Fitted Values", ylab = "et1")
gghistogram(et1)
hist(et1, main = "Histogram of et1", xlab = "et1")
#signifiance for normality-Shapiro-Wilk test for normality- The data follows a normal distribution, p less than 0.05- data not normal
shapiro.test(et1)
#are residuals correlated
acf(et1)
#or
acf(et1, main = "ACF of et1")
#significance test for ACF- Ljung-box- null hypothesis of no autocorrelation-i.i.d
acf_test <- Box.test(et1, type   = "Ljung-Box")
print(acf_test)

##ARCH and GARCH models- Adil Shah- today's volatility depends on volatiltiy of 
##error term-that is other factrs in past(engle),as well as its own volatily-
##bollarsav GARCH(1,1)-defualt lag
#1-conditional, unconditional-doesn't depend on past
#GAARCH-depends on mean as S.D is calculated from ARIMA model only##
#variance=omega+Aplhasquare(lags).,i.e,error+variance(lags)i.e., impact of 
##persistance-volatility clustring##
##data descripton as discussed in starting
##Arch effect
install.packages("rugarch")
library(rugarch)
install.packages("FinTS")
library(FinTS)
install.packages("e1071")
library(e1071)
ArchTest(Exc)
ArchTest(Brent)
ArchTest(Gold)
ArchTest(Sp)
#by default(1,1) lag, here a1 is alpha term(error), a0 is omega(constant),b1
#is beta(variance)
garch(Exc,grad="numerical",trace=FALSE)
garch(Brent,grad="numerical",trace=FALSE)
garch(Gold,grad="numerical",trace=FALSE)
garch(Sp,grad="numerical",trace=FALSE)
#now to forecast the future volatility, create a model and store in a vector,
#here ugarch-univariate garch forecasting model#
rgarch <- ugarchspec(variance.model = list(garchOrder = c(1, 1)), 
                     mean.model = list(armaOrder = c(1,1)))
# Check the specification#
show(rgarch)
rgarchfit=ugarchfit(rgarch,data=Sp)
rgarchfit
#likewsie sGARCH fit model for other variables
rgarchfit=ugarchfit(rgarch,data=Exc)
rgarchfit
## till now-we discussed s-garch only- the suitability of various models can be checked
#through various information criteria and log likelihood(maximum value), or if alpha + beta is greater than 1, asymmetric 
#garch models can be used#
#normally egarch performs best
#diagram for sgarch
news=newsimpact(rgarchfit)
plot(news$zx,news$zy,ylab=news$yexpr,xlab=news$xexpr,type="p",main="news impact curve")
#egarch
egarch <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(1, 1)))
egarch_fit <- ugarchfit(spec = egarch, data = Exc)
egarch_fit
news=newsimpact(egarch_fit)
plot(news$zx,news$zy,ylab=news$yexpr,xlab=news$xexpr,type="p",main="news impact curve")
##for Sp and other variables
egarch_fit <- ugarchfit(spec = egarch, data = Sp)
egarch_fit
news=newsimpact(egarch_fit)
plot(news$zx,news$zy,ylab=news$yexpr,xlab=news$xexpr,type="p",main="news impact curve")

## for other models, just replace eGARCH with gjr garch or pgarch or Tgarch, Igarch...#
#plot

##to get the excel file of conditional volatility from GARCH-
#for recent rgarch vector-Exc
variancegarch=ts(rgarchfit@fit$sigma^2)
options(scipen = 999)
print(variancegarch)
plot.ts(variancegarch)
# Write the extracted variances to a CSV file
write.csv(variancegarch, file = "variancegarch.csv", row.names = FALSE)
##likewise for asymmetric egarch_fit-Sp
varianceegarch=ts(egarch_fit@fit$sigma^2)
print(varianceegarch)
plot.ts(varianceegarch)
# Write the extracted variances to a CSV file
write.csv(varianceegarch, file = "varianceegarch.csv", row.names = FALSE)

##rGARCH forecasting for Exc#####
model_fit <- ugarchfit(data = Exc_ts[1:3500], spec = ugarchspec(order = c(0, 0, 2)))
print(model_fit)
forecast_result <- forecast(model_fit, n.ahead = 24)
print(forecast_result)
plot.ts(forecast_result)
#comparing actual and forecasted results
plot(Exc, col = "yellow", type = "l", lty = 1, ylim = range(c(rprice, forecast_result$mean)))
lines(forecast_result$mean, col = "red", type = "l", lty = 2)
legend("topright", legend = c("Actual", "Forecast"), col = c("yellow", "red"), lty = 1:2)
##likesie for asymmetric models

##Multivariate GARCH modelling-DCC-GARCH-spillovers-helps in porfolio optimization through correlations and
#volatilities; VAR-degree and direction of spillovers
##readxl:: means library(readxl), ensure that the file path is already set
##to import excel data
## to choose a file stored in other location
# or to Choose the Excel file interactively
excel_file <- file.choose()
# Load the Excel file
data <- excel_file <- file.choose()
view(rrmul)
attach(rrmul)
install.packages("zoo")
library(zoo)
library(tseries)
library(rugarch)
library(FinTS)
library(e1071)
library(rmgarch)
# Assuming 'spec1' is your univariate GARCH model specification (replace it with your actual specification)
spec1 <- ugarchspec(mean.model = list(armaOrder = c(0, 0)),
                    variance.model = list(garchOrder = c(1, 1), model = "sGARCH"),
                    distribution.model = "norm")

# Create a multivariate specification- here for two var-exc,brent-rep=2 otherwsie adjust
spec_multivariate <- multispec(replicate(2, spec1))

# Specify the DCC (Dynamic Conditional Correlation) model
dcc_spec <- dccspec(uspec = spec_multivariate, dccOrder = c(1, 1), distribution = "mvnorm")
##to look into the excle file cloulmn names and no. of observations
str(rrmul)
# Fit the DCC model- effect of exc on brent, first part of the result is for arima for exc
##dcca1-short-term spillover, dccb1-long term spillover

fit1=dccfit(dcc_spec, data.frame(Exc,Brent))
fit1

##forcasting dcc volatility
forecast=dccforecast(fit1, n.ahead = 10)
print(forecast)  #we can apply previous codes for getting volatility in an excel file

##hedge ratio-calculate correlation and covariance
##correlation
# Assuming 'fit1' is your fitted DCC model
cor1 <- rcor(fit1)

# Print the conditional correlation matrix- 
print(cor1)
write.csv(cor1, file = "correlation.csv", row.names = FALSE)

##dimensions using the dim function. The dimensions of the matrix represent
#the number of rows and columns, which correspond to the number of assets
#in your multivariate time series.##
dim(cor1)
##overall correlation
cor1[,,dim(cor1)[3]]
##to get all correlations in correlation result- second row first column of all the observations(3524)
cor_result = cor1[2,1,]
View(cor_result)
##export in excel file
write.csv(cor_result, file = "correlationn.csv", row.names = FALSE)
##correlation for volatility
plot.ts(cor_result, col="red")
## similarly fo for covarince, instead of rcor use word rcov
cov1=rcov(fit1)
cov1[,,dim(cor1)[3]]
##to get all covariance in cov_result- second row first column of all the observations(3524)
cov_result = cov1[2,1,]
View(cov_result)
##export in excel file
write.csv(cov_result, file = "covariance.csv", row.names = FALSE)
##correlation for volatility
plot.ts(cov_result, col="red")
##Skewness and Kurtosis??? for my own extensions
##plots with appropriate axis-
##to check if my date column is recognized
class(rrmul$Date)
# Assuming your data frame is named 'rrmul' and the date column is named 'Date'
# If not, replace 'rrmul' and 'Date' with your actual data frame and column names
# Load necessary libraries
install.packages("ggplot2")
library(ggplot2)
install.packages("scales")
library(scales)
# Create a ggplot object
ggplot(rrmul, aes(x = Date, y = cov_result)) +
  
  # Specify the type of plot (e.g., geom_line for a line plot)
  geom_line() +
  
  # Specify date format for the x-axis labels
  scale_x_datetime(labels = date_format("%Y-%m-%d")) +
  
  # Specify labels for axes
  labs(x = "Date", y = "cov_result")


##Adil Shah @IIMA
#Value at risk for a portfolio
#Value at Risk (VaR) is a measure used in finance to estimate the potential 
#loss of an investment portfolio over a specified time horizon and at a given 
#confidence level. Here's a simple example of how you can calculate VaR in R using the historical
#simulation method with an Excel file named "adil" and 
#variables named "clean," "gold," "exc," and "tre."
# Install and load required libraries
install.packages("readxl")
library(readxl)

# Load the data from the Excel file
data <- read_excel("rrmul.xlsx")
View(data)

# Assuming that your data frame has columns named Exc, Brent, Gold, and Sp
Exc_returns <- data$Exc
Brent_returns <- data$Brent
Gold_returns <- data$Gold
Sp_returns <- data$Sp

# Specify the confidence level and time horizon
confidence_level <- 0.95
time_horizon <- 20  # assuming 20 day-1-month, you can adjust it accordingly, time-horizon based on connectedness

# Calculate the portfolio returns - 
#calculate portfoilo weights based on connectedness measure (here,assuming equal weights for simplicity))
portfolio_returns <- 0.25 * Exc_returns + 0.25 * Brent_returns + 0.25 * Gold_returns + 0.25 * Sp_returns


# Calculate the portfolio value at risk (VaR) using the historical simulation method
var_portfolio <- quantile(portfolio_returns, 1 - confidence_level)

# Print the VaR
cat("Value at Risk (VaR) at", confidence_level * 100, "% confidence level for a", time_horizon, "day horizon is:", var_portfolio, "\n")














