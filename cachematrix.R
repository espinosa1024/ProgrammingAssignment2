## Functions to cache inverse of matrix and calculate it if it is not cached


## Function to cache inverse of matrix

makeCacheMatrix <- function(x = matrix()) {

        inv = NULL
        set = function(y) {
                # different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        
        
        inv = x$getinv()
        
        if (!is.null(inv)){
                # get it from the cache and skips the computation
                message("getting cached data")
                return(inv)
        }
        
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        x$setinv(inv)
        
        return(inv)
}
