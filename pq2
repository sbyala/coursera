
## makeCacheMatrix
## Make cache matrix the intitalization of two obj (x, Inv)
## Defines the "behaviors" or functions (Getters, Setters)
## ## return list of functions for matrix

##cacheSolve
## return cached matrix inverse if it's been already computed
## compute inverese of matrix
## # cache matrix


## Write a short comment describing this function
##This function creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- null 
  set <- function(y) { 
    x <<- y
    inv <<- NULL
}
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv 
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  x$setinv(inv)
  inv
}
