## Extension of the matrix data type, allowing the inverse of a
## square matrix to be retrieved quickly if it has been calculated
## before.

## Create an object containing the matrix given as argument,
## a null value to be replaced with the matrix's inverse, and
## setters and getters for both of these values.
##
## If no argument is given, use a 1-by-1 matrix containing NA.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { # Change the matrix and reset the inverse.
        x <<- y
        inv <<- NULL
    }
    get <- function() x # Return the matrix.
    setinv <- function(inverse) inv <<- inverse # Cache the inverse.
    getinv <- function() inv # Return the cached inverse.
    # Return the setters and getters as a list.
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Obtain the inverse of the cache-matrix given as argument, either
## by retrieving it from the cache or by calculating it. If it was 
## calculated, cache it before returning.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(is.null(inv)) { # No cached value
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
    } else {
        message("getting cached data")
    }
    inv
}
