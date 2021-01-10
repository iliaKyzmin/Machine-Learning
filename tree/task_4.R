library(tree)
library(maptree)

all_data <- read.table('data/Lenses.txt')
all_data <- all_data[, -1]
all_data$V6 <- factor(all_data$V6)

model <- tree(V6 ~ ., data = all_data)
print(predict(model, data.frame(V2 = 2, V3 = 1 , V4 =2, V5 = 1)))
