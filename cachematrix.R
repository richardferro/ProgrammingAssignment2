
## This function creates a matrix object so its inverse can be "cahed"

## Sets the value of the matrix object
## Gets the value of matrix object
## Sets the value of the inverse the matrix
## Gets the value of the inverse the matrix
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##This function computes the inverse of special 'matrix' created with makeCacheMatrix
##It first checks to see if the inverse has been calculated.
## If so, this function gets the inverse from cache and skips computation
## otherwise it calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
