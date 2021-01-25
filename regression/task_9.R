library(datasets)

f <- lm(dist ~ speed, cars)
predicted <- predict(f, data.frame(speed = 40.))
print(predicted)

plot(cars$speed, cars$dist, 'l', col = 'red')
lines(cars$speed, cars$speed * f$coefficients[2] + f$coefficients[1], 'l')
