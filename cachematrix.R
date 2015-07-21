## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

## makeCacheMatrix creates a special "Matrix", which is really a list
#containing a function to
#1 set the value of the Matrix
#2 get the value of the Matrix
#3 set the value of the Solve
#4 get the value of the Solve

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolve <- function(sovle) s <<- solve
  getSolve <- function() s
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)

}


## function cacheSolve calculates the inverse of the special "Matrix" created with the above function. 
## It first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix of the data and sets the value in the cache 
## via the setSolve function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setSolve(s)
  s
  
  
}
