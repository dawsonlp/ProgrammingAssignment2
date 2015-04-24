

## Return a matrix wrapped in an object that provides a cached version
## of the output of a function
## When it is initialized, the cache will be NULL, a caching enabled
## function can set the cache to its result, and utilize that cache
## for subsequent calls
## Note the use of <<- to set the value of a variable in the enclosing
## function's environment 

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setmatrix <- function(matrix_in) m <<- matrix_in
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)

}


## This function expects to be passed a caching store for matrices. The first
## time it is called for a particular instance it will pay the full 
## computational cost and calculate the function, but subsequent calls will
## simply return the previously calculated cached result.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}
