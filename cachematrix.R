## This functions cache the inverse of a matrix. Two function is provided the 
## makeCacheMatrix function and the cacheSolve function.

## the makeCacheMatrix function males a list of functions that cache the value of
## the matrix and its inverse
## 1. takes and set the value of matrix
## 2. return and get the value of matrix
## 3. set and cache the inverse of matrix
## 4. get the value of cached inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  
    s <- NULL
    set <- function(y) {
      x <<- y
      s <<- NULL
    }
    get <- function() x

    setsolve <- function(inv) s <<- inv
    getsolve <- function() s
    
    list(set = set, 
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    
}


## cacheSolve function return the inverse of the matrix cached

cacheSolve <- function(x, ...) {

    s <- x$getsolve()
    if(!is.null(s)) {
      message("getting cached data")
      return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
    
  }
