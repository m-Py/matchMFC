library(tinytest)
library(matchMFC)
library(anticlust)

N <- 20
distances <- as.matrix(dist(rnorm(N)))

skew <- 2

n_scales <- 4
stopifnot(N %% n_scales == 0) # much harder in general, if the distribution of scales is arbitrary
scales <- sample(rep_len(1:n_scales, N))
table(scales)

positives <- sample(c(TRUE, FALSE), size = N, replace = TRUE)

sum(!positives)
size <- 2
n_groups <- N/size
uu <- matchMFC(
  distances, size = size,
  scale = scales,
  solver = "glpk",
  time_limit = 5
)
table(uu)


# Function to test if output has feasible structure regarding group sizes
group_size_constraints_met <- function(x, size) {
  tab <- table(x)
  all(tab == size)
}

# Function to test if scales are all in different sets
scales_in_different_sets <- function(x, scales) {
  all(table(scales, x) <= 1)
}

table(uu)
expect_true(group_size_constraints_met(uu, size))
table(scales, uu)
expect_true(scales_in_different_sets(uu, scales)) #cannot be met, oftentimes for arbitrary scale distribution


## Another test case for triplets

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



## Another test case for triplets

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


