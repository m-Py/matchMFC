ilp_to_groups <- function(solution, N) {
  # These steps are taken:
  # 1. Create matrix of inter-item connections from vector of inter-item connections
  # 2. Create clustering vector from matrix; this is returned
  selection_matrix <- matrix(ncol = N, nrow = N)
  selected <- rev(rev(solution$x)[-(1:N)])
  selection_matrix[upper.tri(selection_matrix)] <- selected
  # the following line copies the upper triangular into the lower; diagonal = 0
  selection_matrix <- as.matrix(as.dist(t(selection_matrix)))
  clusters_from_selection_matrix(selection_matrix)
}

# Convert a selection matrix to clustering vector
# Inverse function for `selection_matrix_from_clusters`
#
# param selection_matrix: A N x N matrix where TRUE in cell [i,j]
#     indicates that elements i and j are in the same (anti)cluster
# return: A clustering vector with elements 1, ..., K indicating cluster membership
clusters_from_selection_matrix <- function(selection_matrix) {
  N <- nrow(selection_matrix)
  clusters <- rep(NA, )
  # assign first cluster to set and all its connections
  clusters[1] <- 1
  clusters[which(selection_matrix[1, ] == TRUE)] <- 1
  # Now iterate over the remaining elements
  for (i in 2:N) {
    # test if item i is already in a cluster
    if (!is.na(clusters[i])) {
      next
    }
    next_cluster_index <- max(clusters, na.rm = TRUE) + 1
    clusters[i] <- next_cluster_index
    clusters[which(selection_matrix[i, ] == TRUE)] <- next_cluster_index
  }
  clusters
}

