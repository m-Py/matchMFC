library(tinytest)
library(matchMFC)
library(anticlust)

# define functions to test if constraints are met
group_size_constraints_met <- matchMFC:::group_size_constraints_met
scales_in_different_sets <- matchMFC:::scales_in_different_sets
item_polarities_balanced <- matchMFC:::item_polarities_balanced

## Test 1: Pairings of N = 20; does not consider item polarity

N <- 20
distances <- as.matrix(dist(rnorm(N)))

n_scales <- 4
stopifnot(N %% n_scales == 0) # much harder in general, if the distribution of scales is arbitrary
scales <- sample(rep_len(1:n_scales, N))

size <- 2
n_groups <- N/size
uu <- matchMFC(
  distances, size = size,
  scale = scales,
  solver = "glpk",
  time_limit = 5
)

table(uu)
expect_true(group_size_constraints_met(uu, size))
table(scales, uu)
expect_true(scales_in_different_sets(uu, scales)) #cannot be met, oftentimes for arbitrary scale distribution


## Test 2: Triplets of N = 21; does not consider item polarity

N <- 21
distances <- as.matrix(dist(rnorm(N)))

n_scales <- 3
stopifnot(N %% n_scales == 0) # much harder in general, if the distribution of scales is arbitrary
scales <- sample(rep_len(1:n_scales, N))
table(scales)

positives <- sample(c(TRUE, FALSE), size = N, replace = TRUE)

sum(!positives)
size <- 3
n_groups <- N/size
uu <- matchMFC(
  distances, size = size,
  scale = scales,
  solver = "glpk",
  time_limit = 5
)

# group_size constraints are met
table(uu)
expect_true(group_size_constraints_met(uu, size))
table(scales, uu)
expect_true(scales_in_different_sets(uu, scales)) #cannot be met, oftentimes for arbitrary scale distribution

diversity_objective(distances, uu) #
diversity_objective(distances, categorical_sampling(scales, n_groups)) # not as good as optimized!



## Test 3: Pairings of N = 20; considers item polarity

N <- 20
distances <- as.matrix(dist(rnorm(N)))

n_scales <- 4
stopifnot(N %% n_scales == 0) # much harder in general, if the distribution of scales is arbitrary
scales <- sample(rep_len(1:n_scales, N))
positives <- sample(c(TRUE, FALSE), size = N, replace = TRUE)

size <- 2
n_groups <- N/size
uu <- matchMFC(
  distances,
  size = size,
  scale = scales,
  solver = "glpk",
  time_limit = 5,
  positive_polarity = positives
)

table(uu)
expect_true(group_size_constraints_met(uu, size))
table(scales, uu)
if (scales_in_different_sets(uu, scales)) { #  I assume it may not always work if both scale and polarity constraints are inserted
  expect_true(item_polarities_balanced(uu, n_groups, positives))
} #cannot be met, oftentimes for arbitrary scale distribution

## Test 4: Triplets of N = 21; considers item polarity

N <- 21
distances <- as.matrix(dist(rnorm(N)))

n_scales <- 7
stopifnot(N %% n_scales == 0) # much harder in general, if the distribution of scales is arbitrary
scales <- sample(rep_len(1:n_scales, N))
positives <- sample(c(TRUE, FALSE), size = N, replace = TRUE)

size <- 3
n_groups <- N/size
uu <- matchMFC(
  distances,
  size = size,
  scale = scales,
  solver = "glpk",
  time_limit = 5,
  positive_polarity = positives
)

table(uu)
expect_true(group_size_constraints_met(uu, size))
table(scales, uu)
table(positives, uu)
if (scales_in_different_sets(uu, scales)) { #  I assume it may not always work if both scale and polarity constraints are inserted
  expect_true(item_polarities_balanced(uu, n_groups, positives))
} #cannot be met, oftentimes for arbitrary scale distribution


## Test 5: Qartletts of N = 24; considers item polarity

N <- 24
distances <- as.matrix(dist(rnorm(N)))

n_scales <- 8
stopifnot(N %% n_scales == 0) # much harder in general, if the distribution of scales is arbitrary
scales <- sample(rep_len(1:n_scales, N))
positives <- sample(c(TRUE, FALSE), size = N, replace = TRUE)

size <- 4
n_groups <- N/size
uu <- matchMFC(
  distances,
  size = size,
  scale = scales,
  solver = "glpk",
  time_limit = 5,
  positive_polarity = positives
)

## Problem: May not yield feasible solution during time limit; this case must be dealt with!

table(uu)
expect_true(group_size_constraints_met(uu, size))
table(scales, uu)
table(positives, uu)
expect_true(item_polarities_balanced(uu, n_groups, positives))
expect_true(scales_in_different_sets(uu, scales)) #  I assume it may not always work if both scale and polarity constraints are inserted!!


## Test 6: Triplets of N = 21; considers item polarity; restricts the distribution of negative vs. positive polarity

N <- 21
distances <- as.matrix(dist(rnorm(N)))

n_scales <- 3
stopifnot(N %% n_scales == 0) # much harder in general, if the distribution of scales is arbitrary
scales <- sample(rep_len(1:n_scales, N))
positives <- rep(c(TRUE, FALSE), c(11 ,10))

size <- 3
n_groups <- N/size
uu <- matchMFC(
  distances,
  size = size,
  scale = scales,
  solver = "glpk",
  time_limit = 5,
  positive_polarity = positives,
  n_groups_both_polarity = n_groups - 1
)

table(uu)
expect_true(group_size_constraints_met(uu, size))
table(scales, uu)
expect_true(scales_in_different_sets(uu, scales))
table(positives, uu)
# restricted condition on distribution of negative items met:
expect_true(colSums(table(uu, positives) >= 1)[1] == (n_groups - 1))




N <- 21
distances <- as.matrix(dist(rnorm(N)))

n_scales <- 3
stopifnot(N %% n_scales == 0) # much harder in general, if the distribution of scales is arbitrary
scales <- sample(rep_len(1:n_scales, N))
positives <- rep(c(TRUE, FALSE), c(11 ,10))

size <- 3
n_groups <- N/size
uu <- matchMFC(
  distances,
  size = size,
  scale = scales,
  solver = "glpk",
  time_limit = 5,
  positive_polarity = positives,
  n_groups_both_polarity = n_groups - 1
)

table(uu)
expect_true(group_size_constraints_met(uu, size))
table(scales, uu)
expect_true(scales_in_different_sets(uu, scales))
table(positives, uu)
# restricted condition on distribution of negative items met:
expect_true(colSums(table(uu, positives) >= 1)[1] == (n_groups - 1))




## Test 7: Quartletts of N = 20; considers item polarity; restricts the distribution of negative vs. positive polarity

N <- 20
distances <- as.matrix(dist(rnorm(N)))

n_scales <- 10
stopifnot(N %% n_scales == 0) # much harder in general, if the distribution of scales is arbitrary
scales <- sample(rep_len(1:n_scales, N))
positives <- rep(c(TRUE, FALSE), c(12, 8))

size <- 4
n_groups <- N/size
uu <- matchMFC(
  distances,
  size = size,
  scale = scales,
  solver = "glpk",
  time_limit = 5,
  positive_polarity = positives,
  n_groups_both_polarity = n_groups - 2
)

table(uu)
expect_true(group_size_constraints_met(uu, size))
table(scales, uu)
expect_true(scales_in_different_sets(uu, scales))
table(positives, uu)
# restricted condition on distribution of negative items met:
expect_true(colSums(table(uu, positives) >= 1)[1] == (n_groups - 2))
