library(tinytest)
library(matchMFC)

N <- 15
distances <- as.matrix(dist(rnorm(N)))

skew <- 2

positives <- rep(c(TRUE, FALSE), c(N/2 + skew, N/2 - skew))
is_in_minority_class <- !positives
scales <- as.numeric(as.factor(sample(1:3, size = N, replace = TRUE)))
table(scales)
sum(!positives)
n <- 3
uu <- matchMFC(
  distances, n = n,
  scale = scales,
  solver = "glpk",
  positive_polarity = positives
)

# verify that the three negatively poled items are in separate groups
group_size_constraints_met <- function(x) {
  tab <- table(x)
  all(tab == tab[1])
}

# group_size constraints are met
expect_true(group_size_constraints_met(uu))
expect_true(length(table(uu)) == N/n)
table(uu, positives) # exactly 3 groups have negatively poled items, as they should
