## Cameron Bellamoroso
## Programming Assignment 2

## In this file, functions cache and solve matrices. 
## Matrices that have already been solved (inverted) are stored in the cache
## to decrease processing time.



## This function takes a matrix as an input, solves it, and stores it in the cache

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inver <<- solve
        getinverse <- function() inver
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns inverses of matrices that have been cached.

cacheSolve <- function(x, ...) {
        inver <- x$getinverse()
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data, ...)
        x$setinverse(inver)
        inver
}
