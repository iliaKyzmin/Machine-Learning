library(cluster)
library(gtools)

all_data <- read.table('data/pima-indians-diabetes.data', sep = ',')
true_clusters <- all_data[, 9]
all_data <- all_data[, -9]


get_accuracy <- function(clusters, true_clusters, cl_num = 3) {
  cl_info <- lapply(1:cl_num, function (i) clusters == i)
  permuts <- permutations(cl_num, cl_num, 1:cl_num)
  possible_accuracy <- numeric(nrow(permuts))
  for (i in 1:nrow(permuts)) {
    for (j in 1:cl_num) {
      clusters[cl_info[[j]]] <- permuts[i, j]
    }
    possible_accuracy[i] <- sum(diag(table(clusters, true_clusters))) /
      nrow(all_data)
  }
  return(max(possible_accuracy))
}

clusters_kmeans <- kmeans(all_data, 2)
print(get_accuracy(clusters_kmeans$cluster, true_clusters, 2))
