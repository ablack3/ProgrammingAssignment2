# Below are two functions that are used to create a special object that stores a 
# matrix and caches its inverse. 
# 
#
#
# The first function, makeCacheMatrix, creates a special "matrix" object, 
# which is really a list containing four functions to get and set the value
# of the matrix as well as to get and set the value of the cached inverse matrix.
# The matrix and its cached inverse are stored in the environment in which these 
# functions were defined. 

makeCacheMatrix <- function(x = matrix()) {
        # x is defined in makeCacheMatix's environment
        inv <- NULL
        set <- function(y = matrix()) {
                x <<- y 
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(newInverse) inv <<- newInverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

# The following function calculates the inverse of the special "matrix" object
# created with the above function. However, it first checks to see if 
# the inverse has already been calculated. If so, it gets the inverse from the
# cache and skips the computation. Otherwise, it calculates the mean of the 
# data and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x=list()) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}



