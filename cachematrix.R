## the following two functions are used to cache the inverse of a matrix
## makeCacheMatrix and CacheSolve

## the makeCacheMatrix creates a list that;
## a) sets the value of the matrix
## b) gets the value of the matrix
## c) sets the value of the inverse of the matrix
## d) gets the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        
       
}


## the makeCacheMatrix function checks if the inverse of a matrix has been computed or not .
## if it has not been computes, it does so using the setinverse function.
## and if it has been computed already, it justs skips it.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
## sample run:
## pmatrix <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
## pmatrix$get()
##   [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## pmatrix$getinverse()
## NULL
## cacheSolve(pmatrix)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## pmatrix$getinverse()
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
> 
