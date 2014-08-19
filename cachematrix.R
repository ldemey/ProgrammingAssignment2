## This file contains two functions for quickly calculating 
## the inverse of a matrix by caching. This means that 
## whenever the inverse of a matrix is calculated, it is 
## cached, i.e. stored in memory for potential future use, 
## thereby avoiding having to recalculate it again.

## If we have to calculate the inverse of a matrix, 
## we first check whether its inverse is already in memory 
## (i.e. has already been calculated earlier), and then:
## -- if the inverse is already in memory, we just retrieve 
##    it from memory and are done
##    (without calculating it again ==> efficiency gain!)
## -- if the inverse is not yet in memory, we do calculate
##    it, and store it in memory for potential future use.

##############################################################################################

## The function makeCacheMatrix takes as input an ordinary matrix object, 
## and gives as output a 'self-defined' matrix object that can cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {     # x is the input: an 'ordinary' matrix object
    inv <- NULL                                 # inv is supposed to be the inverse of x
                                                # so far, inv has not yet been calculated (=> NULL)
    set <- function(y) {            
        x <<- y                                 # the set function overwrites the matrix x with a new matrix y
        inv <<- NULL                            # inv is reset to NULL (since it has not yet been calculated 
    }											# for y, i.e. for the new value of x)
    get <- function() x                         # the get function simply returns the 'ordinary' matrix x
    setinverse <- function(inverse) inv <<- inverse # the setinverse function sets inv to inverse
    getinverse <- function() inv                # the getinverse function simply returns inv
    list(set = set, get = get,
         setinverse = setinverse,               # create and return a list of four functions:
         getinverse = getinverse)               # (set, get, setinverse, getinverse)
}                                               # this list will be our 'self-defined matrix object'



## The function cacheSolve takes as input a 'self-defined matrix object' 
## (i.e. the output of makeCacheMatrix), and calculates and returns its inverse via caching.

cacheSolve <- function(x, ...) {        # x is the 'self-defined matrix object'
    inv <- x$getinverse()               # we use the getinverse() function of x to retrieve its inverse
    if(!is.null(inv)) {                 # if this inverse has already been calculated (i.e. cached),
        message("getting cached data")  # we are done: just return the already-calculated inverse
        return(inv)                     # (no need to recalculate it ==> efficiency gain!)
    }
                                        # on the other hand, if the inverse has not yet been calculated...
    data <- x$get()                     # we use get() to retrieve the 'ordinary' matrix representation of x
    inv <- solve(data, ...)             # we actually calculate its inverse, using the solve function from R
    x$setinverse(inv)                   # cache the inverse, so that it is available for potential future use
    inv                                 # return the inverse
}