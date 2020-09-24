# Code

## Algorithm

<p float="left">
  <img src="./pix/tree_grow.png" width="500">
</p>

**tree_grow()** function is written accordingly with the code structure as below:
```
tree_grow <- function(table, nmin, minleaf, nfeat) {
  
  # nodelist <- {{training data}}
  nodelist <- list(table)
  
  # the model of the trained tree
  tree_nodelist <- list()
  tree_index <- 0
  
  repeat{
    
    # current_node <- select node from nodelist
    current_node <- nodelist[[1]]
    # nodelist <- nodelist - current node
    nodelist[[1]] <- NULL
    
    # If impurity(current node) > 0 AND observations(current node) >= nmin, the current node is allowed to be split.
    if(impurity_by_gini_index(current_node[,nfeat+1]) > 0 && length(current_node[,1]) >= nmin) {
      
      # S <- set of candidate splits in current node
      # s* <- argmax {impurity reduction(s, current node)}, where s is in S
      for(feat in 1:nfeat) {
        result <- bestsplit(current_node[,feat], current_node[,nfeat+1])
      }
      
      # child nodes <- apply(s*, current node)
      left_children <- current_node[current_node[,feat.best] <= feat.splitpoints.best,]
      right_children <- current_node[current_node[,feat.best] > feat.splitpoints.best,]
      
      # Check minleaf constraint
      if(length(left_children[,1]) < minleaf || length(right_children[,1]) < minleaf) {
      	# If observations(child nodes) < minleaf
        # Stop splitting, and append the current node as the terminal node to the tree model.
        tree_index <- tree_index + 1
        tree_nodelist[[tree_index]] <- c(NA, NA, vote_of_majority(current_node, nfeat+1)) 
        next
      } else {
        # If observations(child nodes) >= minleaf
        # Split the current node into child nodes, and append the current node as the non-terminal node to the tree model.
        tree_index <- tree_index + 1
        tree_nodelist[[tree_index]] <- c(feat.best, feat.splitpoints.best, NA)
        
        # nodelist <- nodelist + child nodes
        nodelist[[length(nodelist) + 1]] <- left_children
        nodelist[[length(nodelist) + 1]] <- right_children
      }
      
    } else {
      
      if (impurity_by_gini_index(current_node[,nfeat+1]) == 0) {
        # If the current node is pure
        # Append the current node as the terminal node to the tree model by its pure label.
        tree_index <- tree_index + 1
        tree_nodelist[[tree_index]] <- c(NA, NA, current_node[1,nfeat+1])
        
      } else {
      	# If the current node is not pure
        # Append the current node as the terminal node to the tree model by its majority of labels.
        tree_index <- tree_index + 1
        tree_nodelist[[tree_index]] <- c(NA, NA, vote_of_majority(current_node, nfeat+1))
      }
    }
    
    # until nodelist = ∅
    if(length(nodelist) == 0)
      break
  }
  
  # Return the model of the trained tree
  tree_nodelist
}
```

## Tree model

<p float="left">
  <img src="./pix/tree_model.png" width="500">
</p>

