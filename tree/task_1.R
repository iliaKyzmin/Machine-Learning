library(mlbench)
library(tree)
library(maptree)

data(Glass)
model <- tree(Type ~., Glass)
draw.tree(model, cex=0.5)

prunned_model <- prune.tree(model, k = 10)
draw.tree(prunned_model, cex=0.7)
