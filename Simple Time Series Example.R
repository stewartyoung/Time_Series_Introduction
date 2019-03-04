# ***Note this picks up at Part 3 of the README***

# Multiplicative Decomposition of Time Series Models

# Create a vector track of quarters across 4 years
Quarters<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)

# Create a vector to model stock prices across 4 years
StockPrice<-c(3.89414, 3.40792, 1.24523, 4.20107,
              7.50202, 7.11712, 5.1522, 8.63096,
              11.6921, 11.0655, 9.87043, 12.8885,
              15.7568, 15.7817, 13.6428, 16.2162)

plot(x = Quarters, y = StockPrice)