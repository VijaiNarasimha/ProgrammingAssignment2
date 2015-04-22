
## Function to store the matrix which is invertible
## Solve function creates the inverse of a the matrix
makeCacheMatrix <- function(x = matrix()) 
  {
  inverse<- NULL
  set<- function(y)
  {
    x<<- y
    inverse<<- NULL
  }
  get<- function() x
  setInverse<- function(solve)
    inverse<<- solve
  getInverse<- function() inverse
  list( get = get, set = set,
        setInverse = setInverse, getInverse = getInverse)
}


## Function to get the Inverse of the above matrix.
## If already present in memory, print that inverse

cacheSolve <- function(x, ...) 
  {
        ## Return a matrix that is the inverse of 'x'
  inverse = x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
