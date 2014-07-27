## The following functions, modelled on the "Caching the Mean of a Vector"  
## example in Programming Assignment 2 of R Programming, create a special
## "matrix" object that can cache its inverse (in makeCacheMatrix) and 
## then (in cacheSolve) can either compute the inverse of this object or 
## retreive it from the cache, depending on whether the inverse is already 
## in the cache.


## ----

## Function to create a special "matrix" object that can cache its inverse--
## this is really a list containing functions to 
##   1) set the value of the matrix
##   2) get the value of the matrix
##   3) set the value of the inverse
##   4) get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) minv <<- solve(x)
        getinv <- function() minv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## ----

## Function to find inverse of "matrix" object created as above--
## but, first it checks to see if the inverse has already been calculated;
## if so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data matrix and sets the 
## value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        minv <- x$getinv()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setinv(minv)
        minv
}
