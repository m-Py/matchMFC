
#' Construct the ILP represenation of an MFC item assignment problem
#' @param distances An distance object or matrix representing the
#'   distances between items
#' @param p The number of groups to be created
#' @param solver A string identifing the solver to be used ("Rglpk",
#'   "gurobi")
#' @param is_in_minority_class Logical: Is item negatively coded?
#' @param n_leaders_minority numbers of negatively items that should be cluster leaders
#'
#' @return A list representing ILP formulation of the instance
#'
#' @importFrom Matrix Matrix
#'
#' @export
#'
#' @references
#'
#' T. Bulhões, G. F. de Sousa Filho, A. Subramanian, and F. C. Lucídio
#' dos Anjos, “Branch-and-cut approaches for p-cluster editing,”
#' Discrete Applied Mathematics, vol. 219, pp. 51–64, 2017.
#'
#' T. Bulhões, A. Subramanian, G. F. Sousa Filho, and F. C. Lucídio dos
#' Anjos, “Branch-and-price for p-cluster editing,” Computational
#' Optimization and Applications, vol. 67, no. 2, pp. 293–316, 2017.
#'
#' Grötschel, M., & Wakabayashi, Y. (1989). A cutting plane algorithm for a clustering problem.
#' Mathematical Programming, 45, 59-96.
#'
#' Papenberg, M., & Klau, G. W. (2021). Using anticlustering to partition data sets into equivalent parts.
#' Psychological Methods, 26(2), 161--174. https://doi.org/10.1037/met0000301
#'

item_assign_ilp <- function(
    distances, p, solver = "glpk", is_in_minority_class=NULL, n_leaders_minority=NULL) {

  ## This ILP is a mixture of several formulations from the literature, plus an
  ## additional novel constraint that enforces the number of groups that have negatively coded items.
  # a. It is a clique partitioning model using the "traditional" triangular constraints from Grötschel & Wakabayashi (1989).
  # b. It uses additional constraints from #' T. Bulhões, et al. (2017) to enforce the number of item cluster.
  # c. It uses additional constraints from Papenberg & Klau (2021) to enforce the number of items per cluster.
  # d. It uses a novel constraint that uses decision variables from b. to enforce the number of groups that have negatively
  #    coded items. (These additional decision variables are the primary reason why b. is used; actually, c. is already
  #    sufficient to enforce the number of groups.)

  equal_sign <- "=="
  lower_sign <- "<="
  greater_sign <- ">="

  ## Problem: I have matrix of costs but need vector for ILP formulation.
  costs_m <- as.matrix(distances)
  n_items <-  nrow(costs_m)
  group_size = n_items / p
  ## Make vector of costs in data.frame (makes each cost identifiable)
  costs <- expand.grid(1:ncol(costs_m), 1:nrow(costs_m))
  colnames(costs) <- c("i", "j")
  costs$costs <- c(costs_m)
  costs$pair <- paste0("x", paste0(costs$i, "_", costs$j, "_"))
  ## remove all cases where j >= i, i.e. remove redundant or self distances
  costs <- subset(costs, i < j)
  rownames(costs) <- NULL
  ## `costs` now contains the "vectorized" distances.

  ## How many decision variables do I have? Edges + coding of cluster
  ## leadership for each item.
  n_vars <- nrow(costs) + n_items

  ## Compute the number of constraints:
  ## The number of triangular constraints:
  n_tris <- choose(n_items, 3) * 3
  ## The number of constraints for constraint (5): The constraint
  ## numbers are taken from Bulhoes et al. 2017 ("Branch-and-price") (not constraints 9-12)
  n_c5 <- 1 # also use to forbid some items from being leaders
  ## The number of constraints for constraint (6):
  n_c6 <- nrow(costs) ## for each decision var x_ij one constraint
  ## The number of constraints for constraint (7):
  n_c7 <- n_items - 1 ## for each but the first item one constraint
  ## The number of constraints for constraint (8):
  n_c8 <- 1
  ## Number of constraints enforcing the size of the groups:
  n_c9 <- n_items ## c9 not part of Bulhoes et al.'s formulation
  n_c10 <- 1 # one constraint that ensures that negatively poled items are leaders

  m <- sum(is_in_minority_class)
  if (m > p && n_leaders_minority < p) {
    n_c11 <- m # additional constraint that ensure valid results when n_leaders_minority < p
    need_additional_constraints <- TRUE
  } else {
    n_c11 <- 0
    need_additional_constraints <- FALSE
  }

  if (m > p && n_leaders_minority >= p) { ## greater equal does not really make sense, because it can be maximum p
    n_c12 <- m # additional constraint that balances the positively coded items among clusters
    need_additional_constraints2 <- TRUE
  } else {
    n_c12 <- 0
    need_additional_constraints2 <- FALSE
  }


  ## Total number of constraints:
  n_constraints <- n_tris + n_c5 + n_c6 + n_c7 + n_c8 + n_c9 + n_c10 + n_c11 + n_c12

  ## Start constructing the matrix representing the left-hand side of
  ## the constraints. Each column is a decision variable, each row is
  ## a constraint.

  constraints <- matrix(0, ncol = n_vars, nrow = n_constraints)
  colnames(constraints) <- c(costs$pair, paste0("y", 1:n_items))
  ## Use row and column names to identify the decision variables and
  ## the constraints. row names: "tc" are triangular constraints.
  constraint_names <- c(
    paste0("tc", 1:n_tris), "c5",
    paste0("c6_", 1:n_c6),
    paste0("c7_", 1:n_c7),
    "c8", paste0("c9_", 1:n_c9),
    "c_10"
  )
  if (need_additional_constraints) {
    constraint_names <- c(constraint_names, paste0("c11_", 1:n_c11))
  }

  if (need_additional_constraints2) {
    constraint_names <- c(constraint_names, paste0("c12_", 1:n_c12))
  }

  rownames(constraints) <- constraint_names

  ## (1) Triangular constraints

  counter <- 1
  for (i in 1:n_items) {
    for (j in 2:n_items) {
      for (k in 3:n_items) {
        ## ensure that only legal constraints are inserted:
        if (!(i < j) | !(j < k)) next
        ## offset for addressing the data.frame:
        offset <- (counter - 1) * 3
        ## triangular constraint 1
        constraints[offset + 1, paste0("x", i, "_", j, "_")] <- -1
        constraints[offset + 1, paste0("x", i, "_", k, "_")] <- 1
        constraints[offset + 1, paste0("x", j, "_", k, "_")] <- 1
        constraints[offset + 1, paste0("y", k)] <- 1
        ## triangular constraint 2
        constraints[offset + 2, paste0("x", i, "_", j, "_")] <- 1
        constraints[offset + 2, paste0("x", i, "_", k, "_")] <- -1
        constraints[offset + 2, paste0("x", j, "_", k, "_")] <- 1
        ## triangular constraint 3
        constraints[offset + 3, paste0("x", i, "_", j, "_")] <- 1
        constraints[offset + 3, paste0("x", i, "_", k, "_")] <- 1
        constraints[offset + 3, paste0("x", j, "_", k, "_")] <- -1
        ## increase counter
        counter <- counter + 1
      }
    }
  }
  constraints <- insert_group_contraints(constraints, n_items)
  # last constraint: Number of cluster leaders from minority class
  constraints["c_10", ] <- c(rep(0, nrow(costs)), rep(c(1, 0), c(sum(is_in_minority_class), sum(!is_in_minority_class))))

  # last (?) set of constraints: ensure that each negative item is either cluster leader
  # or connected to other negative item (that is cluster leader)
  cn <- colnames(constraints)
  if (need_additional_constraints) {
    for (i in 1:m) {
      nn <- grep(paste0("x", i, "_|_", i, "_"), cn, value = TRUE)
      nn <- grep(paste(m+c(1:(n_items-m)), collapse = "|"), nn, value = TRUE, invert = TRUE) #only items with low indices
      constraints[paste0("c11_", i), nn] <- 1
      constraints[paste0("c11_", i), paste0("y", i)] <- 1
    }
  }

  ## another constraint if n_leaders_minority >= p: ensure that each group has at least one positively coded item
  ## by ensuring that each negatively coded item is connected to at least one positively coded
  ## sum(x_ij) >= 1 (i \in negatively coded items; j \in positively coded items)
  counter <- 1
  if (need_additional_constraints2) {
    for (i in 1:m) { # indices of negatively coded items
      nn <- grep(paste0("x", i, "_|_", i, "_"), cn, value = TRUE)
      nn <- grep(paste("_", (m+1):n_items, sep = "", collapse = "|"), nn, value = TRUE) # x_ij (i \in negatively coded items; j \in positively coded items)
      constraints[paste0("c12_", counter), nn] <- 1
      counter <- counter + 1
    }
  }

  ## Make the to-be-returned constraint matrix take less storage
  ## as a sparse matrix: (TODO: make it sparse from the beginning)
  constraints <- Matrix::Matrix(constraints, sparse = TRUE)

  ## (7) Insert the direction of the constraints:
  equalities = c(
    rep(lower_sign, n_tris),
    rep(equal_sign, n_c5),
    rep(lower_sign, n_c6),
    rep(greater_sign, n_c7),
    rep(equal_sign, n_c8),
    rep(equal_sign, n_c9),
    equal_sign,
    rep(greater_sign, n_c11), # n_c11 can be zero
    rep(greater_sign, n_c12) # n_c12 can be zero
  )

  ## (8) right hand side of the ILP <- many ones
  rhs <- c(
    rep(1, n_tris + n_c5 + n_c6 + n_c7),
    p,
    rep(group_size - 1, n_c9),
    n_leaders_minority,
    rep(1, n_c11), # n_c11 can be zero
    rep(1, n_c12) # n_c12 can be zero
  )

  stopifnot(length(rhs) == length(equalities) && length(rhs) == nrow(constraints))
  ## (9) Construct objective function. Add values for the cluster leader
  ## decision variables to the objective functions. These must be 0
  ## because they are not part of the objective function, but they are
  ## needed for the standard form of the ILP
  obj_function <- c(costs$costs, rep(0, n_items))

  ## give names to all objects for inspection of the matrix
  names(rhs) <- rownames(constraints)
  names(equalities) <- rownames(constraints)
  names(obj_function) <- colnames(constraints)

  ## return instance
  instance              <- list()
  instance$n_groups     <- p
  instance$group_size   <- group_size
  instance$distances    <- distances
  instance$costs        <- costs
  instance$constraints  <- constraints
  instance$equalities   <- equalities
  instance$rhs          <- rhs
  instance$obj_function <- obj_function

  return(instance)
}

## This function inserts constraints concerned with cluster number and cluster
## size
insert_group_contraints <- function(constraints, n_items) {

  ## (2) Constraint (5): First element is cluster leader
  constraints["c5", "y1"] <- 1

  ## (3) Constraint (6): restrictions on cluster leadership
  ## y_j <= 1 - x_ij <=> y_j + x_ij <= 1

  counter <- 1
  for (i in 1:n_items) {
    for (j in 2:n_items) {
      if (i >= j) next
      constraints[paste0("c6_", counter), paste0("x", i, "_", j, "_")] <- 1
      constraints[paste0("c6_", counter), paste0("y", j)] <- 1
      counter <- counter + 1
    }
  }

  ## (4) Constraint (7): more restrictions on cluster leadership
  ## y_j >= 1 - sum(x_ij) <=> y_j + sum(x_ij) >= 1
  counter <- 1
  for (j in 2:n_items) {
    constraints[paste0("c7_", counter), paste0("y", j)] <- 1
    for (i in 1:n_items) {
      if (i >= j) next
      constraints[paste0("c7_", counter), paste0("x", i, "_", j, "_")] <- 1
    }
    counter <- counter + 1
  }

  ## (5) Constraint (8): Specify number of clusters
  constraints["c8", paste0("y", 1:n_items)] <- 1

  ## (6) Add constraint forcing the clusters to be of equal size (I call
  ## it constraint (9) but it is not taken from Bulhoes 2017)

  for (i in 1:n_items) { ## i: current item that may be a cluster leader
    for (j in 1:n_items) {
      ## mark all edges that i is part of (i may be lower or higher index!)
      if (i < j)
        constraints[paste0("c9_", i), paste0("x", i, "_", j, "_")] <- 1
      if (j < i)
        constraints[paste0("c9_", i), paste0("x", j, "_", i, "_")] <- 1
    }
  }
  return(constraints)
}
