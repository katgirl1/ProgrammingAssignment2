## These functions find the inverse of a matrix. If the inverse already
## exists, the cacheSolve function returns the existing matrix instead of
## recalculating it, to save computing time and resources. 

## This function creates a matrix object that can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    invs <- NULL
    set <- function(y){
        x <<- y
        invs <<- NULL
        
    }
    
    get <- function() x
    setinverse <- function(inv) invs <<- inv
    getinverse <- function() invs
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)

}


## This function finds the inverse of the matrix and stores it in the object
## from the first function. If the inverse already exists, that matrix is
## retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invs <- x$getinverse()
    if(!is.null(invs)) {
        message("getting cached data")
        return(invs)
    }
    dat <- x$get()
    invs <- solve(dat, ...)
    x$setinverse(invs)
    invs
}
