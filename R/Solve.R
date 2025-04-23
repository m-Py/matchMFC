
#' Solve an instance of MFC item assignment
#'
#' @param items A data.frame of item features. Rows must correspond to
#'     items and columns to features.
#' @param p How many groups are to be created.
#' @param solver A string identifing the solver to be used ("Rglpk",
#'   "gurobi")
#' @param positives logical - is item positively coded.
#' @param n_leaders_minority Scalar number -
#' @return A data.frame containing one column of item ids (here, the id
#'     corresponds to the order of the items) and one column contains the
#'     group assignments of the items. The original items are also
#'     returned as columns of the data.frame.
#'
#' @export
#'
item_assignment <- function(distances, p, solver, positives, n_leaders_minority) {

  distances <- as.matrix(distances)
  n <- nrow(distances)

  # some sanity checks; make more profound later
  stopifnot(length(n_leaders_minority) == 1)
  stopifnot(length(positives) == n)
  stopifnot(is.logical(positives))
  stopifnot(length(p) == 1)
  stopifnot(n_leaders_minority <= p)

  if (sum(positives) >= (n/2)) {
    is_in_minority_class <- !positives
  } else {
    is_in_minority_class <- positives
  }

  # reorder data set
  orderings <- data.frame(original_order = 1:n, new_order = order(is_in_minority_class, decreasing = TRUE))
  distances <- distances[orderings$new_order, orderings$new_order]
  is_in_minority_class <- is_in_minority_class[orderings$new_order]
  orderings <- orderings[orderings$new_order, ]

  ## 2. Generate ILP model based on reordered data
  cat("Creating model...\n")
  ilp <- item_assign_ilp(
    distances,
    p,
    solver = solver,
    is_in_minority_class = is_in_minority_class,
    n_leaders_minority = n_leaders_minority
  )
  cat("Model done!\n")

  # 3. Solve model
  cat("Starting solving...\n")
  solution <- solve_ilp(ilp, solver)
  print(solution$x)

  orderings$groups <- ilp_to_groups(solution, n)
  orderings$groups[order(orderings$original_order)]
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
