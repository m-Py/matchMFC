## File with functions that test if constraints are met
# Function to test if output has feasible structure regarding group sizes
group_size_constraints_met <- function(x, size) {
  tab <- table(x)
  all(tab == size)
}

# Function to test if scales are all in different sets
scales_in_different_sets <- function(x, scales) {
  all(table(scales, x) <= 1)
}

# Function to test if the item polarities are balanced
item_polarities_balanced <- function(x, n_groups, positives) {
  # Case A: it is possible that in each group there are positives and negatives
  if (sum(positives) >= n_groups && sum(!positives) >= n_groups) {
    return(all(table(x, positives) >= 1))
  }

  if (sum(positives) < sum(!positives)) {
    minority <- positives
  } else {
    minority <- !positives
  }

  all(table(x, minority)[, 1] >= 1)

}
