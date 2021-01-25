library(mlbench)
library(tree)
library(maptree)

set.seed(12345)
data(Glass)
all_data <- Glass[order(runif(nrow(Glass))), ]
nt <- as.integer(nrow(all_data) * 0.8)
train_data <- all_data[1:nt, ]
test_data <- all_data[(nt + 1):nrow(all_data), ]

count_accuracy <- function(model, t_data) {
  predicted <- predict(model, t_data)
  predicted_classes <- apply(predicted, 1, function(row) {which.max(row)})
  true_types = as.integer(as.character(t_data$Type))
  return(sum(diag(table(predicted_classes, true_types))) / nrow(t_data) )
}

model <- tree(Type ~., train_data)
draw.tree(model, cex=0.5)
print(count_accuracy(model, test_data))

prunned_model <- prune.tree(model, k = 10)
draw.tree(prunned_model, cex=0.7)

prunned_model
prunned_model <- snip.tree(prunned_model, nodes = 54)
draw.tree(prunned_model, cex = 0.7)
print(count_accuracy(prunned_model, test_data))
