## This pair of functions calculates and caches the inverse of a matrix.
## Note that it assumes the matrix is always invertible.

##This first function creates a "matrix" object that caches the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setfunc <- function(solve) m <<- solve
    getfunc <- function() m
    list(set = set, get = get,
         setfunc = setfunc,
         getfunc = getfunc)
}


## This second function calculates the inverse of the matrix cached by the first function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getfunc()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setfunc(m)
    m
}
