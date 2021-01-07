library(datasets)
data("EuStockMarkets")

plot(EuStockMarkets[, 1], col = "green", ylab = "Market indices")
lines(EuStockMarkets[, 2], col = "red")
lines(EuStockMarkets[, 3], col = "blue")
lines(EuStockMarkets[, 4], col = "yellow")
legend("topleft", 
       legend=c("DAX", "SMI", "CAC", "FTSE"),
       col=c("green", "red", "blue", "yellow"),
       lty=1:4)

reg_data <- data.frame(DAX = as.vector(EuStockMarkets[, 1]),
                       SMI = as.vector(EuStockMarkets[, 2]),
                       CAC = as.vector(EuStockMarkets[, 3]),
                       FTSE = as.vector(EuStockMarkets[, 4]),
                       date =  as.numeric(time(EuStockMarkets)))

legend(1, 95, legend=c("Line 1", "Line 2"),
       col=c("red", "blue"), lty=1:2, cex=0.8)

f1 <- lm(DAX ~ date, data = reg_data)
print(summary(f1))

f2 <- lm(SMI ~ date, data = reg_data)
print(summary(f2))

f3 <- lm(CAC ~ date, data = reg_data)
print(summary(f3))

f4 <- lm(FTSE ~ date, data = reg_data)
print(summary(f4))

f5 <- lm(DAX + SMI + CAC + FTSE ~ date, data = reg_data)
print(summary(f5))
