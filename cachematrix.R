## CacheMatrix: 
##     Matrix object to support caching its inverse, 
##     consisting of two functions for making an object and finding its inverse
##     

## makeCacheMatrix()
##     Makes a CacheMatrix object,
##     which retains the inverse matrix if once solved. 

makeCacheMatrix <- function(x = matrix()) {
	x_inv <- NULL
	set <- function(xx = matrix()){
		x <<- xx 
		x_inv <<- NULL
	}		
	get <- function() x		
	setinv <- function(inv) x_inv <<- inv
	getinv <- function() x_inv
	list(set = set, get = get,
		 setinv = setinv, getinv = getinv)
}


## cacheSolve()
##     Finds the inverse of a given matrix (CacheMatrix object)
##     This returns the cached inverse if the object has it, 
##     otherwise finds the inverse using solve(), stores it in the object,
##     and returns it.

cacheSolve <- function(cx, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- cx$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    x <- cx$get()
    inv <- solve(x, ...)
    cx$setinv(inv)
    inv
}
