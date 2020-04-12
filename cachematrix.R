## A pair of functions that calculate and cache the inverse of a matrix


## the first function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function can solve the inverse of a matrix produced by makeCacheMatrix, and save it in the list.
## If the matrix inverse has already been calculated, it simply pulls this instead.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Found Cached data, fetching this instead of recalculating.")
        return(m)
    } else {
        message("no cached data found, calculating inverse now.")
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}

