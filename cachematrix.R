## Put comments here that give an overall description of what your
## functions do

## This function produces a list that contains a function to set/get the value of a matrix
## and set/get the value of the inversion of that matrix. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
    
}


## This function computes the inverse of the matrix stored by makeCacheMatrix. If the inverse
## has already been calculated, cacheSolve will retrieve the matrix from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setmean(inv)
        inv
}
