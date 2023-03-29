## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix containing a function to
## set the elements of the Matrix
## get the elements of the Matrix
## set the elements of the Matrix inverse
## get the elements of the Matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(inverse) inv <<- inverse
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
## The following function calculates the inverse of the special Matrix with the above function
## If the inverse is already calculated, the function gets the cache and skips the computation
## otherwise, the inverse of the Matrix is calculated, an stored in the cache by the setinverse func
cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setsolve(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
