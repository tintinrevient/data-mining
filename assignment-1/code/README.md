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
      	# If observations(child nodes) < minleaf, stop splitting
        next
      } else {
        # If observations(child nodes) >= minleaf, nodelist <- nodelist + child nodes
        nodelist[[length(nodelist) + 1]] <- left_children
        nodelist[[length(nodelist) + 1]] <- right_children
      }
      
    }
    
    # until nodelist = ∅
    if(length(nodelist) == 0)
      break
  }
}
```

## Tree model

<p float="left">
  <img src="./pix/tree_model.png" width="500">
</p>

