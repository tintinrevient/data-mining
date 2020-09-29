# setwd("G:/My Drive/UU/Masters/Year_3/Data_mining/assignments")
# pima.dat <- read.delim("pima.txt", sep = ",")
# credit.dat <- read.delim("credit.txt", sep = ",")

# Compute the impurity
impurity <- function(y) {
  p0t <- length(y[y == 0])/length(y)
  p0t*(1-p0t)
}

# Compute all possible splits on the values of a given variable
compute_splits <- function(x) {
  levels_sorted <- sort(unique(x)) # get unique values of the feature (I call them the levels)
  (levels_sorted[1:length(levels_sorted)-1] + levels_sorted[2:length(levels_sorted)])/2
}

# Get the most common value
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
#https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode

# Compute the best split for a given variable
bestsplit <- function(x, y, minleaf = 0) {
  
  splits <- compute_splits(x)
  
  if (length(splits) != 0) {
    imp_pa <- impurity(y) # parent impurity
    children <- sapply(matrix(x[, drop = F]), function(value) ifelse(value > splits, T, F)) # T is a right child, F is a left child
    
    # If there is only one split "sapply" returns a one-dimensional array. 
    # I need a two-dimensional array to work with. The transpose operation makes 
    # this happen for me.
    if (is.null(dim(children))) {
      children <- t(children)
    }
    
    nrc <- rowSums(children) # count the number of Ts, i.e. number of right children
    allowed <- nrc >= minleaf & length(x) - nrc >= minleaf # check if right or left child exceed minleaf
    children <- children[allowed,]
    
    if (!is.null(children)) {
      splits <- splits[allowed, drop = F]
      nrc <- nrc[allowed, drop = F]
      
      if(is.null(dim(children))) { # No "sapply" needed
        
        labels_lc <- y[children == F]
        labels_rc <- y[children]
        
        plc <- (length(x)-nrc)/length(y)
        prc <- nrc/length(y)
        
        imp_lc <- impurity(labels_lc)
        imp_rc <- impurity(labels_rc)
        
      } else {
        labels_lc <- apply(children[, , drop = F], 1, function(child) y[child==F])
        labels_rc <- apply(children[, , drop = F], 1, function(child) y[child])
        
        plc <- sapply(nrc[, drop = F], function(n) (length(x)-n)/length(y))
        prc <- sapply(nrc[, drop = F], function(n) n/length(y))
        
        imp_lc <- sapply(labels_lc[, drop = F], function(labels) impurity(labels))
        imp_rc <- sapply(labels_rc[, drop = F], function(labels) impurity(labels))
        
        # Same problem as above; "sapply" returns one-dimensional arrays when possible.
        # The transpose operation ensures that the arrays have the dimensions I need.
        if (is.null(dim(imp_lc)) | is.null(dim(imp_rc)) | is.null(dim(plc)) | is.null(dim(prc))) {
          imp_lc <- t(imp_lc)
          imp_rc <- t(imp_rc)
          plc <- t(plc)
          prc <- t(prc)
        }
      }
      
      impurity_reductions <- imp_pa - (plc*imp_lc + prc*imp_rc)
      highest_reduction <- max(impurity_reductions)[1]
      index_best_split <- match(highest_reduction, impurity_reductions)[1]
      split <- splits[index_best_split]
      c(split, highest_reduction)
    }
  } else {
    c(NA, NA)
  }
}




# Construct tree
# x: a 2D array with attribute values: each row contains the attribute values of one training example
# y: a 1D array of class labels. Binary 0 or 1. No missing values
# nmin: stop growing tree: minimum number of observations node must contain to split
# minleaf: stop growing tree: minimum number of observations for one node after split
# nfeat: number of features that should be considered each split (draw at random nfeat features and select the best one)
# For normal tree growing nfeat equals the number of features, for random forrest it is less.
# Returns: tree object 

tree_grow <- function(x, y, nmin, minleaf, nfeat) {
  which_feats <- sort(sample(1:ncol(x), nfeat)) # get a random sample of n of the feats
  #  S <- sapply(feats[, which_feats, drop = F], function(feat) bestsplit(feat, labels, minleaf = 1)) # Splits per feat
  nodelist <- list(root = cbind(x[, which_feats, drop = F], y)) # Entire training data frame
  current_location <- 0 # node location, I number possible locations startinf with the root as 0, 1 and 2 for the roots potential children
  # 3,4 and 5,6 for the children of the left and the right child of the root respectively, etcetera
  
  # Some arrays for storing the necessary info for the tree
  split <- c()
  feat_name <- c()
  majority_left <- c() # Is going to be filled with info on which class 
  # is the most prominent among the left side (lesser than) of the split
  majority_right <- c()
  location <- c()
  
  while (length(nodelist)!=0){
    current_node <- nodelist[[1]] # pick the node of the list
    nodelist <- nodelist[-1] # remove the top node
    x <- current_node[,1:ncol(current_node)-1] # set feats according to current node
    y <- current_node[,ncol(current_node)] # set labels according to current node
    
    # if the impurity is 0, further splitting will not improve the tree
    # nmin is the number of observations that a node must contain at least, for it to be allowed to be split.
    if (impurity(y)>0 & nrow(current_node) >= nmin){
      S <- sapply(x[, , drop = F], function(feat) bestsplit(feat, y, minleaf = 1))
      s_star <- S[1,which.max(S[2,])]
      split <- c(split, s_star)
      feat_name <- c(feat_name, names(s_star))
      left_child <- current_node[current_node[,names(s_star)] <= s_star[[1]],]
      right_child <- current_node[current_node[,names(s_star)] > s_star[[1]],]
      nl <- nrow(left_child)
      nr <- nrow(right_child)

      if(nrow(current_node) >= nmin){
        majority_left <- c(majority_left, get_mode(left_child[,ncol(left_child)]))
        majority_right <- c(majority_right, get_mode(right_child[,ncol(right_child)]))
        nodelist <- c(nodelist, list(left_child), list(right_child))
        location <- c(location, current_location)
      } 
    } 
    current_location <- current_location + 1
  }
  
  tree <- data.frame(location, split, feat_name, majority_left, majority_right)
  tree
}

predict_label <- function(observation, tr) {
  node_type <- "root"
  location <- 0
  while (!(node_type == "leaf")) {
    node <- tr[tr["location"] == location,]
    if (observation[node[["feat_name"]]]<=node[["split"]]) {
      location <- 1 + 2 * node[["location"]]
      prediction <- node[["majority_left"]]
    } else {
      location <- 2 + 2 * node[["location"]] 
      prediction <- node[["majority_right"]]
    }
    if (!(location %in% tr[["location"]])) {
      node_type <- "leaf"
    }
  }
  prediction
}


# Predict labels
# A new case is dropped down the tree and assigned to the majority class of each node it passes
# x: 2d array containing attribute values of cases for which prediction is required
# tr: tree object created in tree_grow
# Returns: 1d array with predicted class labels for cases in x
tree_pred <- function(x, tr) {
  predictions <- c()
  for (i in 1:nrow(x)) {
    observation <- x[i,]
    predictions <- c(predictions, predict_label(observation, tr))
  }
  predictions
}

#----------------------------------------------------------------------------------------------

# # Test functions: 
# 
# ## tree_grow
# 
# ### Example input credit
# nmin <- 2
# minleaf <- 1
# nfeat <- ncol(credit.dat)-1
# x <- credit.dat[,1:ncol(credit.dat)-1]
# y <- credit.dat[,ncol(credit.dat)]
# 
# tree_grow(x, y, nmin, minleaf, nfeat)
# 
# ### Example input pima
# nmin <- 20
# minleaf <- 5
# nfeat <- ncol(pima.dat)-1
# x <- pima.dat[,1:ncol(pima.dat)-1]
# y <- pima.dat[,ncol(pima.dat)]
# 
# tree_grow(x, y, nmin, minleaf, nfeat)

## tree_predict
# 
# ### Example input credit
# nmin <- 2
# minleaf <- 1
# nfeat <- ncol(credit.dat)-1
# x <- credit.dat[,1:ncol(credit.dat)-1]
# y <- credit.dat[,ncol(credit.dat)]
# tr <- tree_grow(x, y, nmin, minleaf, nfeat)
# 
# y_hat <- tree_pred(x, tr)

#-----------------------------------------------------------------------------------------------