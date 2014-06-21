## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # inv: inverse of matrix x - is initialized
        inv <- NULL
        
        # set: defines the matrix passed to makeCacheMatrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        # get: returns the matrix passed 
        get <- function() x

        # setInverse: sets an inverse value given by user
        setInverse <- function(inverse) inv <<- inverse

        # getInverse: returns the inverse of matrix
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
       ## Return a matrix that is the inverse of 'x'
       inv <- x$getInverse()
       if(!is.null(inv)) {
                message("getting cached inv of matrix")
                return(inv)
        }
       invM <- x$get()
       inv <- solve(invM,...)
       x$setInverse(inv)
       inv
}
