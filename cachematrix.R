## Functions to cache the inverse of a matrix

## ***********************************************************************
## This function creates a special "matrix" object that can cache 
##  its inverse.
## ***********************************************************************
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    ## set a new matrix to the special matrix object
    set <- function(y) {
        x <<- y
        ## the matrix has changed, so need to drop the cached inverse
        inv <<- NULL
    }
    
    ## get the matrix
    get <- function() x
    
    ## function to store the matrix inverse on cache
    setInverse <- function(inverse) inv <<- inverse
    
    ## function to return the matrix inverse
    getInverse <- function() inv
    
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## ***********************************************************************
## This function computes the inverse of the special "matrix" returned by
##  makeCacheMatrix above. If the inverse has already been calculated
##  (and the matrix has not changed), then the cacheSolve retrieves
##  the inverse from the cache.
## ***********************************************************************
cacheSolve <- function(x, ...) {
    
    ## check if the inverse is cached
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached inverse matrix")
        return(i)
    }
    
    ## get the data and calculate the matrix inverse
    data <- x$get()
    i <- solve(data, ...)
    
    ## store the inverse in cache and return it
    x$setInverse(i)
    i
}


## ***********************************************************************
## Usage example:
## ***********************************************************************

## > m <- makeCacheMatrix(matrix(c(2,4,3,1),nrow=2,ncol=2))

## > cacheSolve(m)
## [,1] [,2]
## [1,] -0.1  0.3
## [2,]  0.4 -0.2

## > cacheSolve(m)
## getting cached inverse matrix
## [,1] [,2]
## [1,] -0.1  0.3
## [2,]  0.4 -0.2

## > m$set(matrix(c(4,3,1,2),nrow=2,ncol=2))

## > cacheSolve(m)
## [,1] [,2]
## [1,]  0.4 -0.2
# [2,] -0.6  0.8
