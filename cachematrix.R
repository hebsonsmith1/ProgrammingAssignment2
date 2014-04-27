## These functions help to cache the inverse of a matrix, rather than
## computing it repeatedly

## makeCacheMatrix creates the cached matrix.  It consists of four functions:
## set (sets the matrix), get (produces the matrix), setinverse (sets the inverse
## of the matrix), and getinverse (produces the inverse of the matrix).

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m          
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve produces the cached matrix inversion.  If that particular matrix 
## has already been called by cacheSolve, it will produce the message "getting
## cached data", and just reproduces that solution again.  If it is given a 
## new matrix, it will compute the inverse as normal.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        else {
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        }
        m
 
}
