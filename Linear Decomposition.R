# ***Note this picks up at Part 3 of the README***

# Linear Decomposition of Time Series Models
# Y= 

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

# NOTE: You can just get the trend line from by doing abline, but above splits out the math
# Calculate the alpha and beta coefficients, where model Y = alpha + beta * time
Beta <- (length(Quarters) * sum(StockPrice * Quarters) - sum(StockPrice)*sum(Quarters))/
  (length(Quarters) * sum(Quarters ^ 2) - sum(Quarters) ^ 2 )
Alpha <- mean(StockPrice) - Beta * mean(Quarters) 

# Now caluclate the trend line Y = = alpha + beta * time
Trend <- Alpha + Beta * Quarters

# Plot the data and add the trend line
# NOTE: You can just get the trend line from by doing abline, but above splits out the math
plot(x= Quarters, y = StockPrice)
lines(Trend, col = "blue")

# Get cyclical trend
Cyclical <- StockPrice - Trend
plot(x= Quarters, y= Cyclical, type="p", pch=16)

# Seprate Cyclical Component by quarter
QuartersInYear = 4
for (i in 1:4){
  if (i==1){
    QuarterOne <- c(Cyclical[i],Cyclical[i + QuartersInYear], Cyclical[i + 2*QuartersInYear], Cyclical[i + 3*QuartersInYear])
  }
  if (i==2){
    QuarterTwo <- c(Cyclical[i],Cyclical[i + QuartersInYear], Cyclical[i + 2*QuartersInYear], Cyclical[i + 3*QuartersInYear])
  }  
  if (i==3){
    QuarterThree<- c(Cyclical[i],Cyclical[i + QuartersInYear], Cyclical[i + 2*QuartersInYear], Cyclical[i + 3*QuartersInYear])
  }
  if (i==4){
    QuarterFour <- c(Cyclical[i],Cyclical[i + QuartersInYear], Cyclical[i + 2*QuartersInYear], Cyclical[i + 3*QuartersInYear])
  }
}

library("ggplot2")
Data <- list(QuarterOne,QuarterTwo, QuarterThree, QuarterFour )
Data <- lapply(Data, function(x) cbind(x = seq_along(x), y = x))
list.names <- c("Q1", "Q2", "Q3", "Q4")
Lines <- sapply(Data, nrow)
Data <- as.data.frame(do.call("rbind", Data))
Data$group <- rep(list.names, Lines)
ggplot(Data, aes(x = x, y = y, colour = group)) +
  theme_bw() +
  geom_point()

# Get alpha and Beta
summary(lm(Cyclical ~ Quarters))

