## The functions below are used to cache the inverse of a matrix, provided that the matrix, x, 
## that is initially supplied in the function is an invertible matrix.

##Similar to the "makeVector" function displayed in the example, the "makeCacheMatrix"
##function creates a matrix, which is a list containing a function to:
##1. set the value of the matrix 
##2. get the value of a matrix
##3. set the value of the inverse of a matrix
##4. get the value of the inverse of a matrix. 

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##The cachesolve function calculates the inverse of the matrix created in the function above.  
##First, though, it checks to see if the inverse has already been calculated and, if so, (and the
##matrix has not changed) it gets the inverse from the cache and skips the computation.  If the 
##inverse has not been calculated, it calculates the inverse of the matrix and sets the value of
##the matrix in the cache via the setinverse function. 
cachesolve <- function(x, ...) { 
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}