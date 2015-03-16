makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  
  set <-function(y) {
    x <<- y
    inversed <<- NULL
  }
  
  get <- function() x
  setInversed <- function(newInversed) inversed <<- newInversed
  getInversed <- function() inversed
  list(set = set, get = get, setInversed = setInversed, getInversed = getInversed)
}

cacheSolve <- function(x, ...) {
        inversed <- x$getInversed()
        if(!is.null(inversed)) {
                message("getting cached data")
                return(inversed)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setInversed(Inversed)
        inversed
}