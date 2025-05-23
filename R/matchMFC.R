#' This is THE function
#' @param x Matrix / data frame, the data input
#' @param size The size of the groups
#' @param scales Vector of length \code{nrow(x)}, scale affiliation of each item
#' @param positive_polarity Logical vector of length \code{nrow(x)}, is each item positively coded?
#' @param solver Optional. The solver used to obtain the optimal method.
#'        Currently supports "glpk", "symphony", "lpSolve" and "gurobi".
#' @param time_limit Time limit in seconds, given to the solver. Default is 120 seconds. Set to 0 for no time limit.
#' @param n_groups_both_polarity Optional, number of groups that must have items with both polarities.
#' @param similarity_matrix Logical scalar, defaults to \code{FALSE}. Is \code{x} a similarity matrix (e.g., correlations).
#'
#' @importFrom anticlust optimal_anticlustering
#' @export
matchMFC <- function(x, size, scales, positive_polarity=NULL, solver = "glpk", time_limit = 120, n_groups_both_polarity=NULL, similarity_matrix = FALSE) {



  if (!is_distance_matrix(x)) {
    x <- dist(x)# Default is Euclidean distance
  }
  x <- as.matrix(x)
  N <- nrow(x)
  if (N %% size != 0) {
    stop("The data set cannot be split in parts of ", size, ".")
  }
  p <- N/size # p = number of groups, is used later

  # all(table(anticlust::categorical_sampling(scales, p), scales) < 2) # should test if scale constraints can be met at all
  # should even work on concatenation scale/polarity



  if (similarity_matrix) {
    objective <- "max"
    warning("@Martin, there is no test for using similarity_matrix = TRUE.")
  } else {
    objective <- "min"
  }

  # include cannot-link constraint between items of same scale:
  scales <- as.numeric(as.factor(scales))
  x[category_vector_to_pairlist(scales)] <- sum(x) + 1

  if (!is.null(positive_polarity)) {
    # distances, p, solver, positives, n_leaders_minority, objective = "min", time_limit = NULL) {
    if (is.null(n_groups_both_polarity)) {
      n_groups_both_polarity <- min(p, sum(!positive_polarity), sum(positive_polarity))
    } else {
      warning("@Martin, there is no input validation for setting the `n_groups_both_polarity` argument.")
    }
    return(item_assignment(
      x, p = p,
      solver = solver,
      positives = positive_polarity,
      n_leaders_minority = n_groups_both_polarity,
      objective = objective,
      time_limit = time_limit
    ))
  } else {
    if (similarity_matrix) {
      x <- -x
    }
    ## anticlust::optimal_anticlustering throws an error when time limit is exceeded.... TODO do differently
    ## and anticlust::balanced_clustering() does not have time_limit argument ...
    ## something needs to change in anticlust to make this work properly (I guess balance_clustering needs time_limit argument)
    return(anticlust::balanced_clustering(x, K = p, method = "ilp", solver = solver))
  }
}


is_distance_matrix <- function(m) {
  m <- as.matrix(m)
  if (nrow(m) != ncol(m)) {
    return(FALSE)
  }
  lower <- m[lower.tri(m)]
  m <- t(m)
  upper <- m[lower.tri(m)]
  all(lower == upper)
}

# helper funciton to convert category labels to pairwise edges
category_vector_to_pairlist <- function(x, duplicated = TRUE) {
  n_clusters <- length(unique(x))
  indices <- lapply(1:n_clusters, function(i) get_partners_(x, i))
  indices <- do.call(rbind, indices)
  indices <- indices[indices[, 1] != indices[, 2], ]
  if (duplicated) {
    indices <- rbind(indices, t(apply(indices, 1, rev))) # use (1, 2) and (2, 1)
    indices[!duplicated(indices), ] # but do not use (1, 2), (2, 1), (1, 2) and (2, 1)
  }
  indices
}

get_partners_ <- function(x, i) {
  relevant_idx <- which(x == i)
  if (length(relevant_idx) == 1) {
    return(NULL)
  }
  t(combn(relevant_idx, 2))
}
