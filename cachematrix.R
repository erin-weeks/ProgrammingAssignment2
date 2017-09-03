##Combined, these two functions take a matrix, cache the matrix, sets up space for the inverse matrix,
##And allows for the matrix to be inverted. ASSUMPTION: ALL MATRICES PROVIDED AS ARGUMENT ARE INVERTIBLE.

## Sets the functions to cache original and inverse matrices

makeCacheMatrix <- function(x = matrix()) {
     y <- NULL
     set <- function(z) {
          x <<- z
          y <<- NULL
     }
     get <- function() x
     setinv <- function(solve) y <<- solve
     getinv <- function() y
     list(set=set, get=get, setinv = setinv, getinv=getinv)
}

## must be called after makeCacheMatrix; checks cache to see if cached location currently holds data;
##Returns cached data if it does; else returns inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     y <- x$getinv()
     if(!is.null(y)) {
          message("getting cached data")
          return(y)
     }
     data <- x$get()
     y <- solve(data,...)
     x$setinv(y)
     y
}
