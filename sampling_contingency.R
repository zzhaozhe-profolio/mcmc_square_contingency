# Use this script to construct a function using MCMC to sample contingency
# tables and get the largest value from the table
#
# Zhe Zhao
# Last Updated: Nov 17th, 2020


# function for MCMC sampling contingency table: -------------------------------
#   return a vector of the largest values
sample_tbl = function(root_tbl, step){
  # check if root table is 5*5
  stopifnot(nrow(root_tbl) == ncol(root_tbl) & nrow(root_tbl)==5)
  # check if the rowsums and colsums are 17
  stopifnot(rowSums(root_tbl) == rep(17, 5))
  stopifnot(colSums(root_tbl) == rep(17, 5))
  
  last_tbl = root_tbl
  
  Y = c()
  for(iter in 1:step){
    # randomly choose four points
    i = sample(seq(5), 1)
    j = sample(seq(5), 1)
    m = sample(seq(5)[-i], 1)
    n = sample(seq(5)[-j], 1)
    
    # they have 1/2 probability to +--+ or -++-
    p = sample(1:2, 1)
    iter_tbl = last_tbl
    if(p==1){
      iter_tbl[i, j] = iter_tbl[i, j] + 1
      iter_tbl[m, j] = iter_tbl[m, j] - 1
      iter_tbl[i, n] = iter_tbl[i, n] - 1
      iter_tbl[m, n] = iter_tbl[m, n] + 1
    } else if(p==2){
      iter_tbl[i, j] = iter_tbl[i, j] - 1
      iter_tbl[m, j] = iter_tbl[m, j] + 1
      iter_tbl[i, n] = iter_tbl[i, n] + 1
      iter_tbl[m, n] = iter_tbl[m, n] - 1
    }
    
    # if any negative value, changes are discarded
    if(any(iter_tbl<0)){
      iter_tbl = last_tbl
    } 
    
    last_tbl = iter_tbl
    
    Y = append(Y, max(iter_tbl))
    
    if(iter %% 1e3 == 0){
      cat(sprintf("iteration reaches %i\n", iter))
    }
  }
  
  return(Y)
  
}


# Run: ------------------------------------------------------------------------
set.seed(2)
steps = 1e6
a = sample(3:4, 16, replace = TRUE)
a = matrix(a, 4, 4)
a = rbind(a, 17-colSums(a))
a = cbind(a, 17-rowSums(a))

Y = sample_tbl(a, steps)

hist(Y)
