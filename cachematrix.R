
#first function: makeCacheMatrix
#sets the value of the matrix
#gets the value of the matrix
#sets the value of the inverse matrix
#gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse_m <- NULL
    set <- function(y) {
      x <<- y
      inverse_m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse_m <<- inverse
    getinverse <- function() inverse_m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

#second functipn : cacheSolve
# it first checks to see if the inverse has already been calculated.
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the 
#value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inverse_m <- x$getinverse()
  if(!is.null(inverse_m)) {
    message("getting cached data")
    return(inverse_m)
  }
  data <- x$get()
  inverse_m <- solve(data,...)
  x$setinverse(inverse_m)
  inverse_m

  
}
