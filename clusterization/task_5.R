library(cluster)

all_data <- read.table('data/seeds_dataset.txt', stringsAsFactors = T)
true_clusters <- all_data[, 8]
all_data <- all_data[, -8]


get_accuracy <- function(clusters, true_clusters) {
  return(sum(diag(table(clusters$cluster, true_clusters))) / nrow(all_data)) 
}

clusters_kmeans <- kmeans(all_data, 3)
print(get_accuracy(clusters_kmeans, true_clusters))

clusters_clara <- clara(all_data, 3, stand = T)
print(get_accuracy(clusters_clara, true_clusters))
