library(datasets)

reg_data <- data.frame(spot = as.numeric(sunspot.year), date = 1700:1988)
f <- lm(spot ~ date, data = reg_data)
print(summary(f)$r.squared)

plot(sunspot.year, col = 'red')
lines(reg_data$date, reg_data$date * f$coefficients[2] + f$coefficients[1], 'l')
