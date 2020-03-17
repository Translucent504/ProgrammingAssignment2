## Functions to optimize repeated matrix inversions by storing the inverse
## of a matrix once it has been calculated and reusing this stored(cached) value
## instead of recomputing.

## Function to create a list that acts as a special matrix object that keeps
## track of its own inverse. 

makeCacheMatrix <- function(input_matrix = matrix()) {
        inverse <- NULL
        set <- function(y) {
                input_matrix <<- y
                inverse <<- NULL
        }
        get <- function() input_matrix
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function to calculate the inverse of a matrix if it has not been calculated
## before, otherwise uses the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x', where x is our special
        ## cache matrix object.
        inv <- x$getinverse()
        if(!is.null(inv)){
                inv
        }
        else{
                inv <- solve(x$get())
                x$setinverse(inv)
                inv
        }
}
