## Put comments here that give an overall description of what your
## functions do
## Cache the inverse of a given matrix by converting it into a "special matrix"

## Write a short comment describing this function
## This function caches the matrix as a special matrix

makeCacheMatrix <- function(x = matrix()) {
  val <- NULL
  set <- function(y) {
    x <<- y
    val <<- NULL
  }
  get <- function() x
  setsolve <- function(inverse) val <<- inverse
  getsolve <- function() val
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## Write a short comment describing this function
## This function returns the inverse of a matrix in the makeCacheMatrix function above

cacheSolve <- function(x, ...) {
  val <- x$getsolve()
  if(!is.null(val)) {
    message("getting cached data")
    return(val)
  }
  data <- x$get()
  val <- solve(data, ...)
  x$setsolve(val)
  val
  ## Return a matrix that is the inverse of 'x'
}
