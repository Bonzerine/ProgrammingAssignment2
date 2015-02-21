## This program examines the working of lexical scoping.
## Here matrix inverse is calculated and is put on global environment which can be
## returned if the matrix stays the same

## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.
## makeCacheMatrix creates a list of function to set and get a matrix
## assignment to the global environment (setinverst) and extract it (getinverse)

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL 
      set <- function(y) { 
            x <<- y
            m <<- NULL
      }
      get <- function() {
            x
      }
      setinverse <- function(solve){
            m <<- solve
      } 
      getinverse <- function() {
            m
      }                         
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}

