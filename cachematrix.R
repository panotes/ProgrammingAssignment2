## makeCacheMatrix create new object that can cache the inverse value
## cacheSolve return the inverse value and kept it in new object created by makeCacheMatrix
##

## makeCacheMatrix 
## input : invertible matrix
## get : get the matrix
## set : set new matrix value, inverse value will be reset
## GetInverse : return cache inverse, return null if not compute yet
## SetInverse : set cache inverse

makeCacheMatrix <- function(x = matrix()) 
{
  
  # initial inverse value 
  cacheInverseValue <- NULL
  
  # internal function
  # return true if x,y is the same matrix
  matequal <- function(x, y)  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
  
  #set function, setting new matrix
  Set <- function(value) 
  {
    #if matrix is not the same, reset internal cache
    if (!matequal(x, value))
    {
      x <<- value
      cacheInverseValue <<- null
    }    
  }
  
  #get internal matrix
  Get <- function() x
  
  #GetInverse
  GetInverse <- function()
  {
    
    cacheInverseValue
  }
  
  SetInverse <- function(value)
  {
    
    cacheInverseValue <<- value
  }
  
  list(set = Set, get = Get,
       GetInverse = GetInverse,
       SetInverse = SetInverse)
}


## create new cache matrix and return the inverse of them

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  cacheMatrix <- makeCacheMatrix(x)
  
  # if get inverse and it never set before, compute the new one
  
  inverse <- cacheMatrix$GetInverse()
  
  if (is.null(inverse))
  {
    inverse<-solve(cacheMatrix$get())
    cacheMatrix$SetInverse(inverse)
  }
  
  inverse
}
