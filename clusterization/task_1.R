library(cluster)

data(pluton)


for (max_iter in seq(2, 18, 4)) {
  clusters <- kmeans(pluton, centers = 3, iter.max = max_iter)
  plot(pluton, col=clusters$cluster)
  print(max_iter)
}
