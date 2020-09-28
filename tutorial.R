#setwd("G:/My Drive/UU/Masters/Year_3/Data_mining/assignments")
#pima.dat <- read.delim("pima.txt", sep = ",")
#credit.dat <- read.delim("credit.txt", sep = ",")

# Compute the impurity
impurity <- function(labels) {
  p0t <- length(labels[labels == 0])/length(labels)
  p0t*(1-p0t)
}

# Compute all possible splits on the values of a given variable
compute_splits <- function(values) {
  levels_sorted <- sort(unique(values)) # get unique values of the feature (I call them the levels)
  (levels_sorted[1:length(levels_sorted)-1] + levels_sorted[2:length(levels_sorted)])/2
}

# Get the most common value
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
#https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode

# Compute the best split for a given variable
bestsplit <- function(values, labels) {
  splits <- compute_splits(values)
  imp_pa <- impurity(labels) # parent impurity
  
  # Empty arrays storing results
  nlc <- rep(NA, length(splits))
  nrc <- rep(NA, length(splits))
  impurity_reductions <- rep(NA, length(splits))
  for (i in 1:length(splits)) {
      split <- splits[i]
      labels_left <- labels[values <= split]
      labels_right <- labels[values > split]
      
      nlc[i] <- sum(values <= split)
      nrc[i] <- sum(values > split)
      
      plc <- nlc[i]/length(labels)
      prc <- nrc[i]/length(labels)
      
      imp_lc <- impurity(labels_left)
      imp_rc <- impurity(labels_right)
      
      impurity_reductions[i] <- imp_pa - (plc*imp_lc + prc*imp_rc)
  }
  highest_reduction <- max(impurity_reductions, na.rm = T)[1]
  index_best_split <- match(highest_reduction, impurity_reductions)[1]
  split <- splits[index_best_split]
  split
}
