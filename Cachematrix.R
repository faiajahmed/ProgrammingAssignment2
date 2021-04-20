makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- NULL
  }
  get <- function() {x}
  setInverse <- function(i) {i <<- inverse}
  getInverse <- function() {i}
  list(set = set, get = get, 
  setInverse=setInverse, 
  getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)){
    message("Getting Cached Data")
    return(i)
  }
  mat <- x$get()
  i <- Solve(mat, ...)
  x$setInverse(i)
  i
}