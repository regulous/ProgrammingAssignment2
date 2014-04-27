## Cache an Inverse Matrix

# Function to create an object to hold matrix and cache its inverse
# Note: *the supplied matrix must be invertible*

makeCacheMatrix <- function(x = matrix()) {
        
        xInv <- NULL
        set <- function(y) {
                x <<- y
                xInv <<- NULL
        }
        get <- function() x
        setInv <- function(solve) xInv <<- solve
        getInv <- function() xInv
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)

}


## Write a short comment describing this function
## The following function is used to get the inverse of a matrix:
## -It first checks to see if the inverse has been calculated before.
## - if not, it calculates the inverse and also sets the inverse to be stored in
## - the cache and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache <- x$getInv()
        if(!is.null(cache)) {
                message("getting cached Inverse")
                return(cache)
        }
        data <- x$get()
        cache <- solve(data,...)
        x$setInv(cache)
        cache
}
