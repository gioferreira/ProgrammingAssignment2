## makeCacheMatrix creates a list of length 4 with the matrix to be inverted as well as functions to set the value of
## the matrix, get that value, set the value of the matrix inverse and get that value


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve applies solve to the element get of the list generated above if it has not been applied before


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        datatoinv <- x$get()
        inv <- solve(datatoinv, ...)
        x$setinv(inv)
        inv
}
