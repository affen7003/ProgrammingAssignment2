## makeCacheMatrix is designed to avoid repeated calculation
## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv<<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
    }
    


## The following function calculates the inverse of the special "matrix" created 
##with the above function. However, it first checks to see if the matrix has already 
##been inversed If so, it gets the matrix from the cache and skips the computation. 
##Otherwise, it inverse the matrix and sets the matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
    }
