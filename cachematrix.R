#These functions create cached matrix object that can return a cached 
#value of matrix inverse if it present in the cache.
#If it is not prsent in the cache, inverse is computed using solve() . 
#This value is saved in the cached matrix object and returned.

## Write a short comment describing this function
#Creates a cached matrix object that can get a matrix, set a matrix
#get the inverse, set the inverse
makeCacheMatrix <- function(x = matrix()) {
  inverseresult <- NULL
  set <- function(y) {
    x <<- y
    inverseresult <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverseresult <<- inverse
  getinverse <- function() inverseresult
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Returns a matrix that is the inverse of 'x'.
## If the inverse is already computed, it's returned from the cache.
## Otherwise, its computed.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
