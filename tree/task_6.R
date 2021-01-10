library(tree)
library(maptree)

train_data <- read.table('data/svmdata4.txt', sep = '\t', header = T, stringsAsFactors = T)
test_data <- read.table('data/svmdata4test.txt', sep = '\t', header = T, stringsAsFactors = T)

model <- tree(Colors ~ ., data = train_data)
draw.tree(model, cex = 0.7)

count_accuracy <- function(model, data) {
  predicted <- predict(model, test_data)
  accuracy = 0.
  for (i in 1:nrow(predicted)) {
    cur_class <- if(predicted[i, 1] > 0.5) 'green' else 'red'
    accuracy <- accuracy + (if(cur_class == as.character(data$Colors[i])) 1. else 0.)
  }
  return(accuracy <- accuracy / nrow(data))
}

print(count_accuracy(model, test_data))
