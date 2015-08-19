## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix - function creates a special type of matrix that can store and return
## a previously calculated inverse of the matrix.  the assumption is that the matrix passed
## in as a parameter is invertible.
## parameters - a matrix x
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve - function to return inverse of a matrix created by the makeCacheMatrix
## function.  if the inverse has not yet been calculated, the function will calculated it.
## otherwise, the previously computed and cached value will be returned.
## parameters - a cacheMatrix x
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

## test functions - returns inverted matrix
testCacheMatrix <- function() {
    x <- matrix(NA, 4, 4)
    x[,1] <- c(1:4)
    x[,2] <- c(0,1,2,0)
    x[,3] <- c(4:1)
    x[,4] <- c(0,4,3,0)
    xx <- makeCacheMatrix(x)
    message("running cacheSolve for the first time...")
    xxx <- cacheSolve(xx)
    print.default(xxx)
    message("running cacheSolve for the second time....")
    xxx <- cacheSolve(xx)
    xxx
}
