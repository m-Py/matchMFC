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
  distances, p = p,
  solver = "Rglpk",
  positives = positives,
  n_leaders_minority = 3
)

# verify that the three negatively poled items are in separate groups
group_size_constraints_met <- function(x) {
  tab <- table(x)
  all(tab == tab[1])
}

 # group_size constraints are met
expect_true(group_size_constraints_met(uu))
expect_true(length(table(uu)) == p)
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
  distances, p = p,
  solver = "Rglpk",
  positives = positives,
  n_leaders_minority = n_leaders_minority
)

# verify that there are negatively poled items are in all groups
expect_true(group_size_constraints_met(uu))
expect_true(length(table(uu)) == p)
falses <- table(uu, positives)[,1]
expect_true(all(falses) > 0)

## there cannot be more cluster leaders than there are groups
expect_error(item_assignment(
  distances, n_groups = p,
  solver = "Rglpk",
  is_in_minority_class = !positives,
  n_leaders_minority = n_leaders_minority +1
))


## PROBLEMATIC?! MORE NEGATIVELY POLED ITEMS THAN CLUSTERS; BUT NOT ALL NEGATIVELY POLED ITEMS MAY BE IN DIFFERENT CLUSTERS!
# WORKS NOW!

N <- 20

distances <- as.matrix(dist(rnorm(N)))

skew <- 3

positives <- rep(c(TRUE, FALSE), c(N/2 + skew, N/2 - skew))
is_in_minority_class <- !positives

sum(!positives)

p <- 4

n_leaders_minority <- 3

uu <- item_assignment(
  distances, p = p,
  solver = "Rglpk",
  positives = positives,
  n_leaders_minority = n_leaders_minority
)
table(uu, positives) ## WORKS!!!!

falses <- table(uu, positives)[,1]
expect_true(sum(falses != 0) == n_leaders_minority)

### Pathological case that NOT LONGER FAILS

distances <- structure(c(0, 0, 1, 2, 0, 2, 0, 1, 0, 1, 1, 1, 0, 2, 0, 0, 0,
0, 1, 0, 0, 0, 0, 1, 2, 0, 2, 0, 1, 0, 1, 1, 1, 0, 2, 0, 0, 0,
0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1,
1, 2, 1, 1, 2, 2, 1, 0, 2, 0, 2, 1, 2, 1, 1, 1, 2, 0, 2, 2, 2,
2, 3, 2, 2, 0, 0, 1, 2, 0, 2, 0, 1, 0, 1, 1, 1, 0, 2, 0, 0, 0,
0, 1, 0, 0, 2, 2, 1, 0, 2, 0, 2, 1, 2, 1, 1, 1, 2, 0, 2, 2, 2,
2, 3, 2, 2, 0, 0, 1, 2, 0, 2, 0, 1, 0, 1, 1, 1, 0, 2, 0, 0, 0,
0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1,
1, 2, 1, 1, 0, 0, 1, 2, 0, 2, 0, 1, 0, 1, 1, 1, 0, 2, 0, 0, 0,
0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1,
1, 2, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1,
1, 2, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1,
1, 2, 1, 1, 0, 0, 1, 2, 0, 2, 0, 1, 0, 1, 1, 1, 0, 2, 0, 0, 0,
0, 1, 0, 0, 2, 2, 1, 0, 2, 0, 2, 1, 2, 1, 1, 1, 2, 0, 2, 2, 2,
2, 3, 2, 2, 0, 0, 1, 2, 0, 2, 0, 1, 0, 1, 1, 1, 0, 2, 0, 0, 0,
0, 1, 0, 0, 0, 0, 1, 2, 0, 2, 0, 1, 0, 1, 1, 1, 0, 2, 0, 0, 0,
0, 1, 0, 0, 0, 0, 1, 2, 0, 2, 0, 1, 0, 1, 1, 1, 0, 2, 0, 0, 0,
0, 1, 0, 0, 0, 0, 1, 2, 0, 2, 0, 1, 0, 1, 1, 1, 0, 2, 0, 0, 0,
0, 1, 0, 0, 1, 1, 2, 3, 1, 3, 1, 2, 1, 2, 2, 2, 1, 3, 1, 1, 1,
1, 0, 1, 1, 0, 0, 1, 2, 0, 2, 0, 1, 0, 1, 1, 1, 0, 2, 0, 0, 0,
0, 1, 0, 0, 0, 0, 1, 2, 0, 2, 0, 1, 0, 1, 1, 1, 0, 2, 0, 0, 0,
0, 1, 0, 0), dim = c(21L, 21L), dimnames = list(c("1", "2", "3",
"4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
"16", "17", "18", "19", "20", "21"), c("1", "2", "3", "4", "5",
"6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16",
"17", "18", "19", "20", "21")))

n <- nrow(distances)
positives <- c(
  FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE,
  FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE
)

p <- n / 3

n_leaders_minority <- min(p, sum(!positives))

start <- Sys.time()
my_triplets_opt2 <- item_assignment(
  distances, p = p, solver = "Rglpk", positives = positives, n_leaders_minority = n_leaders_minority
)
Sys.time() - start # sometimes 7s for tiny instance =( (using GLPK though)

expect_true(group_size_constraints_met(my_triplets_opt2))

table(my_triplets_opt2)
table(my_triplets_opt2, positives)

