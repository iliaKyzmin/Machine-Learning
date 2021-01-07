library(datasets)

plot(sunspot.year)

reg_data <- data.frame(spot = as.numeric(sunspot.year), date = 1700:1988)
res <- lm(spot ~ date, data = reg_data)
print(summary(res))
