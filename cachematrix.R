## makeCacheMatrix and cacheSolve work together to calculate the inverse of an 
## invertible square matrix and cache the result to reduce recalcultions

## makeCacheMatrix takes a matrix and returns a list of functions to set and get
## the matrix and the stored inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(solve) inv <<- solve
        getInv <- function() inv
        list(set = set, get=get, setInv=setInv, getInv=getInv)
}


## cacheSolve takes a makeCacheMatrix object, checks if the inverse to the matrix
## is stored and either accesses the cahced inverse or calculates and stores the 
## inverse. 

cacheSolve <- function(z, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <-z$getInv()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        ans <- z$get()
        inv <- solve(ans, ...)
        z$setInv(inv)
        inv
}
