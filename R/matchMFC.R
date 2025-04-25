#' This is THE function
#' @param x Matrix / data frame, the data input
#' @param n The size of the groups
#' @param scale Vector of length \code{nrow(x)}, scale affiliation of each item
#' @param positive_polarity Logical vector of length \code{nrow(x)}, is each item positively coded?
#' @param solver Optional. The solver used to obtain the optimal method.
#'        Currently supports "glpk", "symphony", "lpSolve" and "gurobi".
#' @param time_limit Time limit in seconds, given to the solver. Default is 120 seconds. Set to 0 for no time limit.
#' @param n_groups_both_polarity Optional, number of groups that must have items with both polarities.
#' @param similarity_matrix Logical scalar, defaults to \code{FALSE}. Is \code{x} a similarity matrix (e.g., correlations).
#'
#' @importFrom anticlust optimal_anticlustering
#' @export
matchMFC <- function(x, n, scale, positive_polarity=NULL, solver = "glpk", time_limit = 120, n_groups_both_polarity=NULL, similarity_matrix = FALSE) {

  if (!is_distance_matrix(x)) {
    x <- dist(x)
  }
  x <- as.matrix(x)
  N <- nrow(x)
  p <- N/n # p = number of groups, is used later

  if (similarity_matrix) {
    objective <- "max"
    warning("@Martin, there is no test for using similarity_matrix = TRUE.")
  } else {
    objective <- "min"
  }

  # include cannot-link constraint between items of same scale:
  x[category_vector_to_pairlist(scale)] <- sum(x) + 1

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
    if (!similarity_matrix) {
      x <- -x # uses anticlustering function (for time_limit argument) but does clustering, so we simple negate the input
    }
    return(anticlust::optimal_anticlustering(x, K = K, objective = "diversity", solver = solver, time_limit = time_limit))
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

## TODO

# helper funciton to convert category labels to pairwise edges
category_vector_to_pairlist <- function(x) {
  n_clusters <- length(unique(x))
  indices <- lapply(1:n_clusters, function(i) get_partners_(x, i))
  indices <- do.call(rbind, indices)
  indices <- indices[indices[, 1] != indices[, 2], ]
  indices <- rbind(indices, t(apply(indices, 1, rev))) # use (1, 2) and (2, 1)
  indices[!duplicated(indices), ] # but do not use (1, 2), (2, 1), (1, 2) and (2, 1)
}

get_partners_ <- function(x, i) {
  t(combn(which(x == i), 2))
}
