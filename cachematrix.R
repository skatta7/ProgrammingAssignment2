# Matrix inversion is a costly computation, If we have a requirement for repeated use of matrix inverse,
#It is better to cache the inverse of a matrix rather than compute it repeatedly. 
#The following two functions are used for this.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
# 1) takes matrix m returned by above function makeCacheMatrix
# 2) return the cached value
cacheSolve <- function(m, ...) {
  inv <- m$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse data.")
    return(inv)
  }
  data <- m$get()
  inv <- solve(data)
  m$setinverse(inv)
  inv
}

## Sample run:
## > x <- matrix(c(1,-1/4,-1/4,1), 2, 2)
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## >