## Create a matrix that can cache its own inverse


## Create get and set methods for the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { #allow user to change x, in which case the inverse of x is unknown
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(xinv) inv <<- xinv # allow user to insert known inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Returns the inverse of a matrix.  If a cached inverse exists, that is returned
cacheSolve <- function(x, ...) {
  #Note: this function only useable by makeCacheMatrix function objects
    inv <- x$getinv() # check whether the inverse has been computed before
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get() 
    inv <- solve(data, ...) # maybe need error handling if the matrix is singular?
    x$setinv(inv) # update the cache with the newly computed inverse
    inv
}

### Not run:
# a = makeCacheMatrix(matrix(c(1,0,0,0,2,0,0,0,3), nrow = 3))
# b = cachesolve(a)
# print(b)
# recalc = cachesolve(a)
# print(d)
### End not run

