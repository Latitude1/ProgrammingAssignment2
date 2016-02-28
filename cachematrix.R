## The function "makeCacheMatrix" creates four functions in order to create a special "matrix" object that caches the inverse.
## These four functions are set, get, setinverse, and getinverse. 
## The set function changes the vector. 
## The get function returns x. 
## The setinverse function changes the vector into an inverse. 
## The getinverse function will return the inverse of setinverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## The function "cacheSolve" gets the inverse of matrix returned by "makeCacheMatrix".
## If the inverse is already made with the matrix not changed, "cacheSolve" gets the inverse from the cache.
## If not made, data will get the matrix from "makeCacheMatrix".
## The function m makes the inverse.
## The x$setinverse(m) function will store the inverse in "m" in "makeCacheMatrix".

cacheSolve <- function(x, ...) {
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