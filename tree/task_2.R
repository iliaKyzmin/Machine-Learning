library(DAAG)
library(tree)
library(maptree)
data('spam7')

set.seed(12345)
all_data <- spam7[order(runif(nrow(spam7))), ]
nt <- as.integer(nrow(all_data) * 0.8)
train_data <- all_data[1:nt, ]
test_data <- all_data[(nt + 1):nrow(all_data), ]

model <- tree(yesno ~ ., data = train_data)
draw.tree(model)

count_accuracy <- function(model, data) {
  predicted <- predict(model, data)
  accuracy = 0.
  for (i in 1:nrow(predicted)) {
    cur_class <- if(predicted[i, 1] > 0.5) 'n' else 'y'
    accuracy <- accuracy + (if(cur_class == as.character(data$yesno[i])) 1. else 0.)
  }
  accuracy <- accuracy / nrow(data)
}

print(count_accuracy(model, test_data))

prunned_info <- prune.tree(model, method = "misclass")
print(prunned_info$k)
for (i in 2:4) {
  prunned_model <- prune.tree(model, k = prunned_info$k[i], method = "misclass")
  draw.tree(prunned_model)
  print(count_accuracy(prunned_model, test_data)) 
}
