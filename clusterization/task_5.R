library(cluster)
library(gtools)

all_data <- read.table('data/seeds_dataset.txt', stringsAsFactors = T)
true_clusters <- all_data[, 8]
all_data <- all_data[, -8]


get_accuracy <- function(clusters, true_clusters, cl_num = 3) {
  cl_info <- lapply(1:cl_num, function (i) clusters == i)
  permuts <- permutations(cl_num, cl_num, 1:cl_num)
  
  possible_accuracy <- numeric(nrow(permuts))
  for (i in 1:nrow(permuts)) {
    for (j in 1:cl_num) {
      clusters[cl_info[[j]]] <- permuts[i, j]
    }
    possible_accuracy[i] <- sum(diag(table(clusters, true_clusters))) / nrow(all_data) 
  }
  
  return(max(possible_accuracy))
}

clusters_kmeans <- kmeans(all_data, 3)
print(get_accuracy(clusters_kmeans$cluster, true_clusters))

clusters_clara <- clara(all_data, 3, stand = T)
print(get_accuracy(clusters_clara$cluster, true_clusters))
