## The following 2 functions can compute and cache the inverse of a matrix

## 1 makeCacheMatrix(): 
##   creates a special “matrix” object that can cache its inverse.
##   creates a list containing a function to:
##     1 set the value of the matrix
##     2 get the value of the matrix
##     3 set the value of the inverse of the matrix
##     4 get the value of the inverse of the matrix

makeCacheMatrix <- function(mtrx = matrix()) {
        inverse <- NULL
        set <- function(y) {
            mtrx <<- y
            inverse <<- NULL
        }
        get <- function() mtrx
        setinverse <- function(inverse) inverse <<- inverse
        getinverse <- function() inverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## 2 cacheSolve(): 
##   computes the inverse of the “matrix” returned by makeCacheMatrix()
##   If the inverse has already been calculated and 
##   the matrix has not changed, 
##   it’ll retrieves the inverse from the cache directly.

cacheSolve <- function(mtrx, ...) {
        inverse <- mtrx$getinverse()
        if(!is.null(inverse)) {
            message("Getting cached data..")
            return(inverse)
        }
        mdata <- mtrx$get()
        inverse <- solve(mdata)
        mtrx$setinverse(inverse)
        return(inverse)
        ## Return a matrix that is the inverse of 'x'
}

