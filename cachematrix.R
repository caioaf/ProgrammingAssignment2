## Script contains methods to create special matrix objects and 
## to calculate the inverse of said matrix

## Creates an object with a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse value of a matrix
## If inverse was not cached yet, it also calculates and caches it
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
