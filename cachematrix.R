#creates a 'structured' matrix with setters and getters

makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    
    set <- function(new_mtr) {
        x <<- new_mtr
        inv <<- NULL 
    }
    get <- function() x 
    
    setInverse <- function(new_inv) inv <<- new_inv
    getInverse <- function() inv
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# returns the inverse of matrix if this inverse is already
# stored in cache, otherwise it computes the inverse

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached inverse matrix data")
        return(inv)
    }
    
    inv <- solve(x$get(), ...)
    x$setInverse(inv)
    inv
}