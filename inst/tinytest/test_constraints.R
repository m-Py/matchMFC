library(tinytest)

## extreme skew: fewer negatively poled items than there are groups:

N <- 20

distances <- as.matrix(dist(rnorm(N)))

skew <- 7

positives <- rep(c(TRUE, FALSE), c(N/2 + skew, N/2 - skew))
is_in_minority_class <- !positives

sum(!positives)

p <- 4

distances <- as.matrix(distances)

uu <- item_assignment(
  distances, n_groups = p,
  solver = "Rglpk",
  is_in_minority_class = !positives,
  n_leaders_minority = 3
)

# verify that the three negatively poled items are in separate groups
table(uu, positives)
expect_true(colSums(table(uu, positives))[1] == 3)


## less extreme skew - there can be a negatively poled item in every group

N <- 20

distances <- as.matrix(dist(rnorm(N)))

skew <- 3

positives <- rep(c(TRUE, FALSE), c(N/2 + skew, N/2 - skew))
is_in_minority_class <- !positives

sum(!positives)

p <- 4

n_leaders_minority <- min(sum(!positives), p)

uu <- item_assignment(
  distances, n_groups = p,
  solver = "Rglpk",
  is_in_minority_class = !positives,
  n_leaders_minority = n_leaders_minority
)

# verify that there are negatively poled items are in all groups
falses <- table(uu, positives)[,1]
expect_true(all(falses) > 0)

## there cannot be more cluster leaders than there are groups
expect_error(item_assignment(
  distances, n_groups = p,
  solver = "Rglpk",
  is_in_minority_class = !positives,
  n_leaders_minority = n_leaders_minority +1
))
