#These functions can help calculate matrix inversions, then save the answer in
##cache and call them out if the same calculation is needed.


# setting up the function lists that stores results in cache.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
    x <<- y
    inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv_value) inv <<- inv_value
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This is the function that calculates the inversion. 
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
        ## Return a matrix that is the inverse of 'x'
