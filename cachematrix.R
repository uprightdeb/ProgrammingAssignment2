## These two functions cache the inverse of a 
## matrix, calling if already calculated or calculate to cache.

## This function creates a special matrix variable so that the inverse can be cached.
makeCacheMatrix <- function(x = matrix()) {
        m<- NULL
        set<- function(y) {
                x<<- y
                m<<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m<<-inverse
        getinverse <- function() m
        list(set=set,
             get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## This function calculates the inverse of the special matrix variable or retrieves it 
## if cached.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
