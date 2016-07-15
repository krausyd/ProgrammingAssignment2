## KGF 20160715

## This function creates and chaches the inverse matrix of a provided matrix
## for this, it creates a list of functions for the original matrix
## two of them are for getting and setting the inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This will get the inverse of a cached matrix (this function receives a makeCacheMatrix
## as arg. If the inverse was already calculated and in cache, it will return it. If it 
## is not set, then it will calculate, and the store it into cache using the functions 
## provided by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  
  data<-x$get()
  i<- solve(data, ...)
  x$setinverse(i)
  i
}
