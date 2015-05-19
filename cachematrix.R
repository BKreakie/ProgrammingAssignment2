###R-Programming Assignmnet #2
##Betty Kreakie
##Edit: 2015-05-15
## Code for functions that will cache the inverse of a matrix

## Makes a special matrix object that can caches its inverse.  It is a actually
#a list of functions:
#         
#         1.  set the matrix
#         2.  get the matrix
#         3.  set set the inverse 
#         4.  get the the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Computes the inverse of a matrix if not already cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
