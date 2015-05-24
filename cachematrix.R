## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to:
##      1. set the matrix
##      2. get the matrix
##      3. set the inverse
##      4. get the inverse
## this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix ().
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        inv = x$getinv()
        
        # if the inverse has been calculated, get the data from cache
        
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        # else, calculate the inverse 
        
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # set the value of the inverse calling setinv()
        x$setinv(inv)
        return(inv)

}

## End of assignment 2