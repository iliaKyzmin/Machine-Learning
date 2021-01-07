reg_data <- read.csv('data/reglab1.txt', sep='\t', stringsAsFactors = T)
f1 = lm(z ~ ., data=reg_data)
summary(f1)

f2 = lm(x ~ ., data=reg_data)
summary(f2)

f3 = lm(y ~ ., data=reg_data)
summary(f3)
