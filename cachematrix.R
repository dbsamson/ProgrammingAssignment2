## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #declare an empty matrix
  cachedCopy <- matrix(NA,2,2)
  
  set <- function(y) {
  ## clears the cached copy and passes the result to the calling function. 
    cachedCopy <<- matrix(NA,2,2)
    x<<- y
  }
  get <- function() x
  setinverse <- function(inverse) { cachedCopy <<- inverse }
  getinverse <- function() { cachedCopy }
  list(set= set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## Returns a matrix that is the inverse of 'x'.
## We let R solve for the result inverse matrix, by providing the identity 
## matrix.   
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <-  solve(data,diag(length(data[1,])))
  x$setinverse(m)
  m
}
