library(datasets)

f <- lm(dist ~ speed, cars)
predicted <- predict(f, data.frame(speed = 40.))
print(predicted)
