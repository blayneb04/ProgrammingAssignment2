## The functions below cache and compute the 
## inverse of a martrix

## The function below generates a special "matrix" object
## that can cache the inverse of itself.

makeCacheMatrix <- function(mtx = matrix()) {
        inverse <- NULL
        set <- function(x) {
                mtx <<- x;
                inverse <<- NULL;
        }
        get <- function() return(mtx);
        setinv <- function(inv) inverse <<- inv;
        getinv <- function() return(inverse);
        return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## The function below calculates the inverse of the special
## "matrix" object returned by "makeCacheMatrix". Once the inverse is  
## calculated, "cacheSolve" will return the inverse from the cache.
## Note: the matrix must remain unaltered

cacheSolve <- function(mtx, ...) {
        inverse <- mtx$getinv()
        if(!is.null(inverse)) {
                message("Getting cached data...")
                return(inverse)
        }
        data <- mtx$get()
        invserse <- solve(data, ...)
        mtx$setinv(inverse)
        return(inverse)
}