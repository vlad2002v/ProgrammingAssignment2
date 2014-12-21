## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        ## This will initialize the inverse property
        m <- NULL
        ## This will set the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## This will get the matrix
        get <- function() {
                x
        }
        ## This will set the inverse of the matrix
        setInverse <- function(inverse) {
                m <<- inverse
        }
        ## This will get the inverse of the matrix and 
        ## return the inverse property
        getInverse <- function() {
                m
        }
        ## This will return a list of the methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

## cacheSolve function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then 
## the cachesolve function should retrieve the 
## inverse from the cache.
## Assumed that the matrix supplied is always invertible. 

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ## Return the inverse if its already set
        if(!is.null(m)){
                message("getting cached data")
                ## This will return the matrix
                return(m)
        }
        else {
                m <- solve(x$get())
                ## This will set the inverse to the object
                x$setInverse(m)
                ## This will return the matrix
                return(m)
        }
}



