## This pair of functions allow you to create a matrix, cache
## the inverse of that matrix, and calculates the inverse of that
## matrix if it does not already exist in the cache or retrieves
## it from the cache if it already exists. 

######################################################################
## makeCacheMatrix is a function that creates a special matrix and 
##    caches the inverse of that matrix
##
## Functions/Methods:
##  get/set methods for data set
##  get/set [compute] methods for Inverse
######################################################################

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) inv <<- inverse
    
    getInverse <- function() inv 
    
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

######################################################################
## cacheSolve is a function that calculates the inverse of the a 
## matrix that is returned by the makeCacheMatrix function above. 
##
## Inverse of matrix
##
## Check the cache and return inverse if calculated already
## If doesn't exist in cache, compute the inverse of the matix 
##    and save to cache
######################################################################

cacheSolve <- function( x, ...) {

    # Get values from cache
    inv <- x$getInverse()

    if(!is.null(inv))
    {
        message("getting cached data")
        return(inv)
    }

    ## Calculate the inverse of matrix and cache the result...
    mat <- x$get()
    inv <- solve(mat,...)
    x$setInverse(inv)
    inv
}
