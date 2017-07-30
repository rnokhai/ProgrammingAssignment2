## makeCacheMatrix creates a vector, containing setter and getter functions.
## puts the calculated inverse of a matrix into cache

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)


}


## return the inverse of a matrix; for a new matrix, perform the calculation and place it in cache(setsove)
## If the cached matrix is called upon again, 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  # First, a attempt is made to retrieve the cached data
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached inversed matrix")
    return(m)
  }
  
  # If no chached data is found, the inverse of the matrix is calculated and placed in cache (for next time retrieval)
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  
  # return the inverse of the matrix
  m
  
}
