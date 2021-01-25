reg_data <- read.csv('data/cygage.txt', sep = '\t')
f <- lm(calAge ~ Depth, data = reg_data, weights = reg_data$Weight)
summary(f)$r.squared
