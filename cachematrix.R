## The functions below create a matrix, compute its inverse, and cache it


## The following function creates a list containing functions to set a matrix, get its value, 
## find its inverse, and get its inverse 

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## The following function computes the inverse of the matrix created by the previous function.
## It does so by firstly checking if the inverse has already been computed. If it has, function returns the cached inverse.
## If not, function computes it and caches it for possible further use.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}

