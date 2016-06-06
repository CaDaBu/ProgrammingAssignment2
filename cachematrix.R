## Below are two functions that are used to create a 
## special object that stores a numeric matrix and
## cache's its inverse. 

## The first function creates a special matrix,
## which is really a list containing a function to
## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the value of the inverse
## 4 get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## The following function calculates the inverse of
## the special "matrix" created with the above function.
## However, it first checks to see if the inverse has
##already been calculated. If so, it gets the inverse
##from the cache and skips the computation.
## Otherwise, it calculates the mean of the data
## and sets the value of the mean in the cache via the
## setmean function. 

cacheSolve <- function(x, ...) {
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}
