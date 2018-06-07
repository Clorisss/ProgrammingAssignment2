## Put comments here that give an overall description of what your
## functions do

## Create a list of closures containing the cache
makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    get <- function()
        x
    setcache <- function(p)
        cache <<- p
    getcache <- function()
        cache
    list(
        set = set,
        get = get,
        setcache = setcache,
        getcache = getcache
    )
}


## Write a short comment describing this function

## Solve the matrix and cache the result
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cache <- x$getcache()
    if (!is.null(cache)) {
        message("retrieve the inverse from the cache")
        return(cache)
    }
    data <- x$get()
    cache <- solve(data, ...)
    x$setcache(cache)
    cache
}
