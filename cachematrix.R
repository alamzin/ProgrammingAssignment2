## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix comprises of 4 functions:
## set(y) - resets a cached matrix and saves new matrix
## get() - gets the saved matrix
## setinverse(inverted) - save an inverted matrix
## getinverse() - get the inverted matrix 

makeCacheMatrix <- function(x = matrix()) {
    matrix <- NULL
    set <- function (y) {
        x <<- y
        matrix <<- NULL
    }
    get <- function() x
    setinverse <- function (inverted) matrix <<- inverted
    getinverse <- function () matrix
    list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    
}



## cacheSolve solves a matrix provided or returns cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    matrix <- x$getinverse()
    
    if (!is.null(matrix)) {
        message("found cached data, returning cached matrix")
        return(matrix)
    }
    
    data <- x$get()
    matrix <- solve (data, ...)
    x$setinverse(matrix)
    matrix
    
}

