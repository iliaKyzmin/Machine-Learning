tmp <- read.csv('data/UKgas.csv')

quarter1 <- numeric(nrow(tmp) / 4)
quarter2 <- numeric(nrow(tmp) / 4)
quarter3 <- numeric(nrow(tmp) / 4)
quarter4 <- numeric(nrow(tmp) / 4)

for (i in 0:(nrow(tmp) / 4 - 1)) {
  quarter1[i + 1] <- tmp$UKgas[4 * i + 1]
  quarter2[i + 1] <- tmp$UKgas[4 * i + 2]
  quarter3[i + 1] <- tmp$UKgas[4 * i + 3]
  quarter4[i + 1] <- tmp$UKgas[4 * i + 4]
}

reg_data <- data.frame(Quarter1 = quarter1, 
                       Quarter2 = quarter2,
                       Quarter3 = quarter3,
                       Quarter4 = quarter4,
                       date = 1960:1986)

plot(reg_data$date, reg_data$Quarter1, col = "green", ylab = "Gas", xlab = 'Year', 'l')
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
plot(reg_data$date, reg_data$Quarter1, 'l', col = 'green', xlab = 'Time', ylab = 'Quarter 1')
lines(reg_data$date, reg_data$date * f1$coefficients[2] + f1$coefficients[1], 'l')

predicted2 <- predict(f2, data.frame(date = 2016))
print(paste0('Quarter 2 - ', predicted2))
plot(reg_data$date, reg_data$Quarter2, 'l', col = 'red', xlab = 'Time', ylab = 'Quarter 2')
lines(reg_data$date, reg_data$date * f2$coefficients[2] + f2$coefficients[1], 'l')

predicted3 <- predict(f3, data.frame(date = 2016))
print(paste0('Quarter 3 - ', predicted3))
plot(reg_data$date, reg_data$Quarter3, 'l', col = 'blue', xlab = 'Time', ylab = 'Quarter 3')
lines(reg_data$date, reg_data$date * f3$coefficients[2] + f3$coefficients[1], 'l')

predicted4 <- predict(f4, data.frame(date = 2016))
print(paste0('Quarter 4 - ', predicted4))
plot(reg_data$date, reg_data$Quarter4, 'l', col = 'yellow', xlab = 'Time', ylab = 'Quarter 4')
lines(reg_data$date, reg_data$date * f4$coefficients[2] + f4$coefficients[1], 'l')

predicted5 <- predict(f5, data.frame(date = 2016))
print(paste0('Year - ', predicted5))
plot(reg_data$date, reg_data$Quarter1 + reg_data$Quarter2 + reg_data$Quarter3 + reg_data$Quarter4, 
     'l', col = 'grey', xlab = 'Time', ylab = 'Year')
lines(reg_data$date, reg_data$date * f5$coefficients[2] + f5$coefficients[1], 'l')
