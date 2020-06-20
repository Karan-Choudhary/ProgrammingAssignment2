## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL ##initializing
  set <- function(y){
    x <<- y ##seting value
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ##created
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then 
##the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
  if(!is.null(inv)){ ##checking conditons
    message("getting cached data") ##printing message if conditions gets ture
    return(inv) ##returning inv if condition gets true
  }
  data <- x$get() ##setting value in data
  inv <- solve(data) ##setting value of inv from solve function which has data argument
  x$setInverse(inv)
  inv      ##returning a inverse matrix
}
