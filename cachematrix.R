## The combination of these two functions will
## a) produce the inverse of a given matrix
## b) cache the inverse so that it will be returned
## in the future without additional calculations

## this function contains four others:
## set the matrix, get the matrix, 
## set inverse of matrix, and get inverse of matrix

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


## This function looks to verify the value m, stored previously with getinverse. 
## exists and is not NULL. 
## If it exists in memory, it returns "getting cached data" & the value m


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
