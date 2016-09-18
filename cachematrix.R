# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        #initially set to NULL
        inv <- NULL
        
        #set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        #get the value of the matrix
        get <- function() x
        
        #set the value of inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
        
        #get the value of inverse of the matrix
        getinverse <- function() inv
        
        #make into a list
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

#We assume that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #getting the current state of the inverse 
        inv <- x$getinverse()
        
        #if it has been computed, return the state
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        
        #if it has not been been computed, get the matrix
        data <- x$get()
        
        #find the inverse
        inv <- solve(data,...)
        
        #caching result in the object
        x$setinverse(inv)
        
        #returning new result
        return(inv)
}
