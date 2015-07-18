#library(logging)
#basicConfig()

## cachematrix.R
## There are 2 functions that when used in conjunction, allow a user to create and manipulate
## a matrix and a cached inverse of the matrix. This combination reduces the time necessary 
## to calculate the inverse multiple times.  


## makeCacheMatrix encapsulates an R matrix (passed in via x), and keeps state of matrix' inverse.
##  The function itself implements functions to:
##     1. set the value of the matrix
##     2. get the value of the matrix
##     3. set the value of the inverse
##     4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(new_inv) {
    inv <<- new_inv
  }
  getinv <- function() {
    inv
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Given the output of the function makeCacheMatrix, 'cacheSolve' returns a matrix
## That is the inverse of the original matrix. It first checks to see if the inverse
## has been previously calculated. If so it returns that value. Otherwize, it calculates
## the inverse, and caches via the function setinv
cacheSolve <- function(x, ...) {

  inv <- x$getinv()
  if(!is.null(inv)) { 
    # if cached, return it
    return(inv)
  } else { 
    # if not cached, calculate it, cache it, and return it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
  }
}
