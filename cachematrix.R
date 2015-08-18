library("MASS")

## These functions are used to calculates the inverse of a matrix
## stores the result to avoid computations if the result is needed
## again by some other function to make the function more efficient

## The function, `makeCacheMatrix` creates a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## Reads a list created by `makeCacheMatrix` function,
## Checks whether Inverse is calculated or not,
## If calculated , avoids computation and returns the value 
## else computes inverse and stores it to avoid recomputing.

## NOTE: Uses `ginv` function in the package MASS for inverse calculation

cacheSolve <- function(x, ...) {
      i <- x$getinv()
      if(!is.null(i)) {
            message("getting cached data...")
            return(i)
      }
      data <- x$get()
      i <- ginv(data, ...)
      x$setinv(i)
      i
}
