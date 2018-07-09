## The makeCacheMatrix is a function that gets a matrix and builds a list of functions to store the matrix itself and 
## its already calculated inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y = matrix()) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The cacheSolve function gets the data stored in the makeCacheFuntion and checks to see if the inverse matrix of m has been calculated already.
## If not, it calculates the inverse matrix and stores it in the list created previously 
## at the end it returns the inverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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