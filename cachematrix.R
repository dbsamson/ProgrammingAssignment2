#  These functions calculate the inverse of an invertible matrix, only if 
#  the value isn't already stored in memory.  

#   makeCacheMatrix contains 4 nested functions which act on a persistently 
#     stored variable (cachedCopy).
makeCacheMatrix <- function(x = matrix()) {
  # declare an empty matrix (before calculating the inverse, we check to see
  # if it's already cached)
  cachedCopy <- matrix(NA,1,1)

  # clears the cached copy
  set <- function(y) {
    x<<- y
    cachedCopy <<- matrix(NA,2,2)
  }

  # Retrieves the passed-in parameter
  get <- function() x
  
  # After calculation, stores the result into the persisent variable
  setinverse <- function(inverse) { cachedCopy <<- inverse }
  
  # Retrieves the persistent variable 
  getinverse <- function() { cachedCopy }
  
  # Builds the list of functions on this vector
  list(set= set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}

## Returns a matrix that is the inverse of 'x'.
## We let R solve for the result inverse matrix, by providing the identity 
## matrix.   
cacheSolve <- function(x, ...) {

  # If the result is already calculated, we don't calculate it again. 
  m <- x$getinverse()
  if(!is.na(m[1,1])) {
    message("getting cached data")
    return(m)
  }
  
  # This does the actual calculation of the inverse, and saves it for 
  # subsequent calls to the function.
  data <- x$get()
  m <-  solve(data,diag(length(data[1,])))
  x$setinverse(m)
  m
}
