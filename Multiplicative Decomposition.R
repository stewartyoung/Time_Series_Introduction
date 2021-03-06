# ***Note this picks up at Part 3 of the README***

# Multiplicative Decomposition of Time Series Models


# Create a vector track of quarters across 4 years
Quarters<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)

# Create a vector to model stock prices across 4 years
StockPrice<-c(3.89414, 3.40792, 1.24523, 4.20107,
              7.50202, 7.11712, 5.1522, 8.63096,
              11.6921, 11.0655, 9.87043, 12.8885,
              15.7568, 15.7817, 13.6428, 16.2162)


# Plot the data points
plot(x = Quarters, y = StockPrice)

# Add a line of best fit
abline(lm(StockPrice~Quarters), col="red", lwd=2)


# Calculate the alpha and beta coefficients, where model Y = alpha + beta * time
# NOTE: You can just get the trend line from by doing abline, but above splits out the math
Beta <- (length(Quarters) * sum(StockPrice * Quarters) - sum(StockPrice)*sum(Quarters))/
  (length(Quarters) * sum(Quarters ^ 2) - sum(Quarters) ^ 2 )
Alpha <- mean(StockPrice) - Beta * mean(Quarters) 
 
# Now caluclate the trend line Y = = alpha + beta * time
Trend <- Alpha + Beta * Quarters

# Plot the data and add the trend line
plot(x= Quarters, y = StockPrice)
lines(Trend, col = "red")

StockOverTrend <- StockPrice/Trend

# Plot Y/Trend to leave just the cyclical component
# NOTE: type = "o" plots dots and line connection
plot(x= Quarters, y = StockOverTrend, type = "o")


# To get cyclical trend,  we need to get the average of each quarter
QuartersInYear <- 4
FirstQuarterSum <- 0
SecondQuarterSum <- 0 
ThirdQuarterSum <- 0 
FourthQuarterSum <- 0
for (i in 0:3){
  FirstQuarterSum = FirstQuarterSum + StockOverTrend[1 + i*QuartersInYear]
  SecondQuarterSum = SecondQuarterSum + StockOverTrend[2 + i*QuartersInYear]
  ThirdQuarterSum = ThirdQuarterSum + StockOverTrend[3 + i*QuartersInYear]
  FourthQuarterSum = FourthQuarterSum + StockOverTrend[4 + i*QuartersInYear]
}
OneYearCycle <- c(FirstQuarterSum/QuartersInYear, SecondQuarterSum/QuartersInYear, ThirdQuarterSum/QuartersInYear, FourthQuarterSum/QuartersInYear)
YearlyCycle <- rep(OneYearCycle, times=4)

# Get the predictions, Trend * Cyclical
Prediction <- Trend * YearlyCycle

# Plot the StockPrices with Predictions overlaid
par(mfrow = c(1,2))
plot(x= Quarters , y = StockPrice, pch=16)
lines(Prediction, col="red")

plot(x= Quarters , y = StockPrice, pch=16, col = "orange")
points(x = Prediction, pch=16, col="blue")


# Predict the 17th quarter stock price
Trend[17] <- Alpha + Beta * 17
YearlyCycle[17] <- OneYearCycle[1]
Prediction[17] <- Trend[17] * YearlyCycle[17]
Quarters[17] <- 17 

# Plot stock prices with the predicted 17th Quarter price
# NOTE: Need to use ggplot as the vectors are now of differing lengths
# install.packages("ggplot2")
library("ggplot2")

Data <- list(StockPrice, Prediction)
Data <- lapply(Data, function(x) cbind(x = seq_along(x), y = x))
list.names <- c("StockPrice", "Prediction")
LinesToPlot <- sapply(Data, nrow)
Data <- as.data.frame(do.call("rbind", Data))
Data$group <- rep(list.names, LinesToPlot)
ggplot(Data, aes(x = x, y = y, colour = group)) +
  theme_bw() +
  geom_point()
