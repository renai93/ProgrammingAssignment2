## This script will cache the inverse of a matrix

## Function to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL
        set <- function(y){
                x <<- y
                invM <<- NULL
        }
        get <- function() {x}
        setInv <- function(inverse) {invM <<- inverse}
        getInv <- function() {invM}
        list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## Compute inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getInv()
        if(!is.null(invM){
                return(invM)
        }
        mat <- x$get()
        invM <- solve(mat,...)
        x$setInv(invM)
}
