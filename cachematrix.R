## makeCacheMatrix and cacheSolve functions create a default matrix and 

## The makeCacheMatrix function sets default values for the matrix and its inverse (default value of the inverse is NULL).

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function checks to see if an inverse is already stored for the matrix. 
## If the inverse is not stored (it is NULL), it computes the inverse of the matrix and stores the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        inverse <- x$get()
        i <- solve(inverse, ...)
        x$setinverse(i)
        i
}
