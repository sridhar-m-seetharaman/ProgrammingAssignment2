## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        iom <- NULL  ## inverse of matrix (iom) cache variable
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setmatinv <- function(invofmat) iom <<- invofmat
        getmatinv <- function() iom
        list(set = set, get = get,
             setmatinv = setmatinv,
             getmatinv = getmatinv)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        iom <- x$getmatinv()
        if(!is.null(iom)) {
                message("getting cached data")
                return(iom)
        }
        data <- x$get()
        iom <- solve(data, ...)
        x$setmatinv(iom)
        iom
}

## For Testing results 
## mm %*% solve(mm) == diag(nrow = nrow(mm), ncol = ncol(mm))