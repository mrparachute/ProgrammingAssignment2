## The 2 functions belows work together so that the first cache a matrix and its
## inverse if it has been calculated in the past while the seconddoes the calculation
## with the solve function

## This function creates a list which contains the original matrix and we'll be
## used in cacheSolve

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


## This function takes a list and check if the inverse of a matrix has been 
## calculated before, if not calculate the inverse matrix with the solve
## function and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
