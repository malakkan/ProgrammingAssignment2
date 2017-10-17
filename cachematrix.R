## The aim of those functions is to cache the inverse of a matrix

## The first function "makeCacheMatrix" create a special "matrix" 
## that can cache its inverse
## It consists in a list of functions allowing to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix 
## 4. get the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  set <- function(y) {
    x <<- y
    invX <<- NULL
    
  }
  get <- function() x
  setinv <- function(inverse) invX <<- inverse
  getinv <- function() invX
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The "cacheSolve function calculates the inverse of the special "matrix" 
## created with the "above "makeCacheMatrix" function. It first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse matrix 
## from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the matrix through the function "solve" and sets the value of the 
## inverse matrix in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  invX <- x$getinv()
  if(!is.null(invX)) {
    message("getting cached data")
    return(invX)
  }
  data <- x$get()
  invX <- solve(data, ...)
  x$setinv(invX)
  invX
}
