library(datasets)
tmp <- JohnsonJohnson

quarter1 <- numeric(length(tmp) / 4)
quarter2 <- numeric(length(tmp) / 4)
quarter3 <- numeric(length(tmp) / 4)
quarter4 <- numeric(length(tmp) / 4)

for (i in 0:(length(tmp) / 4 - 1)) {
  quarter1[i + 1] <- as.numeric(tmp[4 * i + 1])
  quarter2[i + 1] <- as.numeric(tmp[4 * i + 2])
  quarter3[i + 1] <- as.numeric(tmp[4 * i + 3])
  quarter4[i + 1] <- as.numeric(tmp[4 * i + 4])
}

reg_data <- data.frame(Quarter1 = quarter1, 
                       Quarter2 = quarter2,
                       Quarter3 = quarter3,
                       Quarter4 = quarter4,
                       date = 1960:1980)

plot(reg_data$date, reg_data$Quarter1, col = "green", ylab = "Profit", xlab = 'Year', 'l')
lines(reg_data$date, reg_data$Quarter2, col = "red")
lines(reg_data$date, reg_data$Quarter3, col = "blue")
lines(reg_data$date, reg_data$Quarter4, col = "yellow")
legend("topleft", 
       legend=c("Quarter 1", "Quarter 2", "Quarter 3", "Quarter 4"),
       col=c("green", "red", "blue", "yellow"),
       lty=1:4)


f1 <- lm(Quarter1 ~ date, data = reg_data)
f2 <- lm(Quarter2 ~ date, data = reg_data)
f3 <- lm(Quarter3 ~ date, data = reg_data)
f4 <- lm(Quarter4 ~ date, data = reg_data)
f5 <- lm(Quarter1 + Quarter2 + Quarter3 + Quarter4 ~ date, data = reg_data)


predicted1 <- predict(f1, data.frame(date = 2016))
print(paste0('Quarter 1 - ', predicted1))

predicted2 <- predict(f2, data.frame(date = 2016))
print(paste0('Quarter 2 - ', predicted2))

predicted3 <- predict(f3, data.frame(date = 2016))
print(paste0('Quarter 3 - ', predicted3))

predicted4 <- predict(f4, data.frame(date = 2016))
print(paste0('Quarter 4 - ', predicted4))

predicted5 <- predict(f5, data.frame(date = 2016))
print(paste0('Year - ', predicted5))
