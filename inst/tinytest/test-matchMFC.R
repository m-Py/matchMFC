library(tinytest)
library(matchMFC)

### First test: without constraint on positive vs negative items

N <- 20
distances <- as.matrix(dist(rnorm(N)))

skew <- 2

scales <- as.numeric(as.factor(sample(1:3, size = N, replace = TRUE)))
table(scales)
#todo if cannot-link constraints can be fulfilled at all (anticlust:::optimal_cannot_link)

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
# verify that the three negatively poled items are in separate groups
group_size_constraints_met <- function(x) {
  tab <- table(x)
  all(tab == tab[1])
}

# group_size constraints are met
expect_true(group_size_constraints_met(uu))
expect_true(all(table(uu) == size))
table(scales, uu)
expect_true(all(table(scales, uu) <= 1)) #cannot be met, oftentimes!


## Now include positive vs. negative items


N <- 27
distances <- as.matrix(dist(rnorm(N)))

skew <- 2

positives <- sample(c(TRUE, FALSE), size = N, replace = TRUE)
scales <- as.numeric(as.factor(sample(1:3, size = N, replace = TRUE)))
table(scales)
#todo if cannot-link constraints can be fulfilled at all (anticlust:::optimal_cannot_link)

sum(!positives)
size <- 3
n_groups <- N/size
uu <- matchMFC(
  distances, size = size,
  scale = scales,
  solver = "glpk",
  time_limit = 5,
  positive_polarity = positives
)
table(uu)
# verify that the three negatively poled items are in separate groups
group_size_constraints_met <- function(x) {
  tab <- table(x)
  all(tab == tab[1])
}

# group_size constraints are met
expect_true(group_size_constraints_met(uu))
expect_true(all(table(uu) == size))
table(uu, positives)
table(scales, uu)
all(table(scales, uu) <= 1) #cannot be met, oftentimes!
