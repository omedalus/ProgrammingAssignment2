## Put comments here that give an overall description of what your
## functions do

## Creates an object that stores a matrix along with its inverse. This object
## can be passed to the cacheSolve function, which will get/set the cached
## inverse of the matrix. This is more computationally efficient than
## computing the inverse every time it's requested.

makeCacheMatrix <- function(x = matrix()) {
        # Private ember variable that will store the inverse of the x matrix.
        cached.inverse <- NULL

        set <- function(y) {
                x <<- y

                # Now that we've changed the matrix stored in this object,
                # our cached inverse is invalid.
                cached.inverse <<- NULL
        }

        get <- function() { 
                x 
        }

        setinverse <- function(inverse) {
                cached.inverse <<- inverse
        }
        
        getinverse <- function() {
                cached.inverse
        }

        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Returns the inverse of the matrix stored in x, which should be an object
## created by makeCacheMatrix. Uses x's cached inverse if it's available,
## or computes a fresh inverse if it's not.

cacheSolve <- function(x, ...) {
        # Ask the x object for its inverse. This action does not actually
        # compute the inverse; it merely retrieves the x object's
        # cached.inverse member.
        inverse <- x$getinverse()

        if(!is.null(inverse)) {
                # Cache hit.
                # The x object has a cached inverse from a previous call
                # to this method. Hooray. Return the cached value.
                message("getting cached data")
                return(inverse)
        }

        # Cache miss. We have to compute the x object's matrix's inverse.
        data <- x$get()
        inverse <- solve(data, ...)
        
        # Cache our solution inside x so we won't have to perform this
        # potentially expensive computation again.
        x$setinverse(inverse)
        
        # Return the computed value.
        inverse
}
