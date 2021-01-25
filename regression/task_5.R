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

f1 <- lm(DAX ~ date, data = reg_data)
print(summary(f1)$r.squared)
plot(reg_data$date, reg_data$DAX, 'l', col = 'green', xlab = 'Time', ylab = 'DAX')
lines(reg_data$date, reg_data$date * f1$coefficients[2] + f1$coefficients[1], 'l')

f2 <- lm(SMI ~ date, data = reg_data)
print(summary(f2)$r.squared)
plot(reg_data$date, reg_data$SMI, 'l', col = 'red', xlab = 'Time', ylab = 'SMI')
lines(reg_data$date, reg_data$date * f2$coefficients[2] + f2$coefficients[1], 'l')

f3 <- lm(CAC ~ date, data = reg_data)
print(summary(f3)$r.squared)
plot(reg_data$date, reg_data$CAC, 'l', col = 'blue', xlab = 'Time', ylab = 'CAC')
lines(reg_data$date, reg_data$date * f3$coefficients[2] + f3$coefficients[1], 'l')

f4 <- lm(FTSE ~ date, data = reg_data)
print(summary(f4)$r.squared)
plot(reg_data$date, reg_data$FTSE, 'l', col = 'yellow', xlab = 'Time', ylab = 'FTSE')
lines(reg_data$date, reg_data$date * f4$coefficients[2] + f4$coefficients[1], 'l')

f5 <- lm(DAX + SMI + CAC + FTSE ~ date, data = reg_data)
print(summary(f5)$r.squared)
plot(reg_data$date, reg_data$FTSE + reg_data$DAX + reg_data$SMI + reg_data$CAC, 
     'l', col = 'grey', xlab = 'Time', ylab = 'ALL')
lines(reg_data$date, reg_data$date * f5$coefficients[2] + f5$coefficients[1], 'l')
