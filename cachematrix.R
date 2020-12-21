## Put comments here that give an overall description of what your
## functions do

## This function produces a list that contains a function to set/get the value of a matrix
## and set/get the value of the inversion of that matrix. 

makeCacheMatrix <- function(x = matrix()) {
        
        # assigns the value NULL to inv, which will hold the inverted matrix
        inv <- NULL
        
        # creates the set function and assigns argument y to argument x
        set <- function(y) {
            x <<- y
            
            # since this assigns a matrix to x but does not compute anything, inv is still NULL
            inv <<- NULL
        }
        
        # creates the get function, returns whatever is stored in x
        get <- function() x
        
        # creates a function to set the value for the inverted matrix, stored in inv
        setinverse <- function(inverse) inv <<- inverse
        
        # creates a function to get the value of the inverted matrix, stored in inv
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
    
}


## This function computes the inverse of the matrix stored by makeCacheMatrix. If the inverse
## has already been calculated, cacheSolve will retrieve the matrix from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        # if there is already an inverted matrix cached, it will be assigned to inv 
        inv <- x$getinverse()
        
        # if there is already an inverted matrix cached, it will be assigned to inv 
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        
        # if there is no inverted matrix cached, it will be computed from the uninverted matrix
        matrix <- x$get()
        inv <- solve(matrix, ...)
        
        # caches the inverted matrix after it has been solved
        x$setinverse(inv)
        
        # returns the inverted matrix
        inv
}
