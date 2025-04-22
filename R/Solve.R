
#' Solve an instance of MFC item assignment
#'
#' @param items A data.frame of item features. Rows must correspond to
#'     items and columns to features.
#' @param n_groups How many groups are to be created.
#' @param solver A string identifing the solver to be used ("Rglpk",
#'   "gurobi")
#' @param standardize Boolean - should the feature values be
#'     standardized before groups are created? Defaults to FALSE.
#' @param heuristic Set the level of "heuristicism" by setting a numeric
#'   value of 0 to 3. Set to 0 to obtain the exact solution for the item
#'   assignment instance. Levels 1 to 3 will in a first step identify
#'   very similar items and then ensure that these will be assigned to
#'   different groups. Level 1 does this preclustering using exact
#'   cluster editing; Level 2 does this preclustering using a heuristic
#'   based on k-means clustering. On Level 3, preclustering is also done
#'   using the heuristic based on k-means clustering, and the final
#'   assignment is no longer done using exact ILP item assignment, but
#'   instead using a simple random assignment.
#'
#' @return A data.frame containing one column of item ids (here, the id
#'     corresponds to the order of the items) and one column contains the
#'     group assignments of the items. The original items are also
#'     returned as columns of the data.frame.
#'
#' @export
#'
item_assignment <- function(distances, n_groups, solver, is_in_minority_class, n_leaders_minority) {

  ## 1. "Cleverly" order the data set so that the constraints on item polings are valid
  n <- length(is_in_minority_class)
  m <- sum(is_in_minority_class)
  positions_old <- which(is_in_minority_class)


  ## THIS DOES NOT WORK AS INTENDED
  stopifnot(n_leaders_minority <= n_groups)
  if (n_leaders_minority > n_groups) { # more potential group leaders than there are groups
    positions_new <- c(1:n_leaders_minority, (n-(m-n_leaders_minority-1)):n) # the first elements go to the front, the others to the tail
  } else {
    positions_new <- 1:n_leaders_minority
  }

  print(positions_new)


  # reorder input matrix
  distances <- as.matrix(distances)
  tmp <- distances[positions_old, positions_old]
  distances[positions_old, positions_old] <- distances[positions_new, positions_new]
  distances[positions_new, positions_new] <- tmp

  tmp <- is_in_minority_class[positions_old]
  is_in_minority_class[positions_old] <- is_in_minority_class[positions_new]
  is_in_minority_class[positions_new] <- tmp


  ## 2. Generate ILP model based on reordered data
  cat("Creating model...\n")
  ilp <- item_assign_ilp(
    distances,
    n_groups,
    solver = solver,
    is_in_minority_class = is_in_minority_class,
    n_leaders_minority = n_leaders_minority
  )
  cat("Model done!\n")

  # 3. Solve model
  cat("Starting solving...\n")
  solution <- solve_ilp(ilp, solver)
  print(solution$x)
  groups <- ilp_to_groups(ilp, solution)

  cat("Solved\n")

  # 4. Restore original order before returning solution
  tmp <- groups[positions_old]
  groups[positions_old] <- groups[positions_new]
  groups[positions_new] <- tmp
  groups
}

#' Solve the ILP formulation of the item assignment problem
#'
#' Usually it will be advised to call the higher level function
#' `item_assignment` By setting the `objective` parameter to "min", this
#' function solves weighted cluster editing instead of item assignment.
#'
#' @param ilp An object representing the ILP formulation of the
#'     instance, returned by `item_assign_ilp`
#' @param solver A string identifing the solver to be used ("Rglpk",
#'   "gurobi")
#' @param objective A string identifying whether the objective function
#'     of the ILP should be maximized ("max") or minimized
#'     ("min"). Maximizing creates similar groups (i.e., solves item
#'     assignment), minimizing creates distinct clusters (i.e., solves
#'     cluster editing).
#'
#' @return A `list` having two entries: `x` is the vector of optimal
#'   coefficients for all decision variables. `obj` is the optimal
#'   objective value.
#'
#' @export
#'

solve_ilp <- function(ilp, solver, objective = "min") {

  ret_list <- list() # return the optimal value and the variable assignment

  if (solver == "Rglpk") {
    max <- FALSE
    if (objective == "max")
      max <- TRUE
    start <- Sys.time()
    ilp_solution <- Rglpk::Rglpk_solve_LP(obj = ilp$obj_function,
                                   mat = ilp$constraints,
                                   dir = ilp$equalities,
                                   rhs = ilp$rhs,
                                   types = "B",
                                   max = max)

    ret_list$x <- ilp_solution$solution
    ret_list$obj <- ilp_solution$optimum
    end <- Sys.time()
    print(end - start)
  } else if (solver == "gurobi") {
    ## build model
    model <- list()
    model$A          <- ilp$constraints
    model$obj        <- ilp$obj_function
    model$modelsense <- objective
    model$rhs        <- ilp$rhs
    model$sense      <- ilp$equalities
    model$vtypes     <- "B"
    ## solve
    ilp_solution <- gurobi::gurobi(model)
    ret_list$x <- ilp_solution$x
    ret_list$obj <- ilp_solution$objval
  }
  ## name the decision variables
  names(ret_list$x) <- colnames(ilp$constraints)
  return(ret_list)
}
