## Contains functions to cache the inverse of a matrix. This is done 
## for performance reasons as the solve function to calculate the 
## inverse of a matrix can be slow for larger matrices. 
## usage, create a matrix:
##      mt <- matrix(runid(100), 10)
## create new object with matrix:
##      mc <- makeCacheMatrix(mt)
## get the inverse of the matrix:
##      mi <- cacheSolve(mc)     

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        src_matrix <- x
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        src <- function() src_matrix
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             src = src,
             setinv = setinv,
             getinv = getinv)
}


## Checks if the inverse of the matrix has already been created. 
## If not the inverse is calculated with solve and cached in the 
## result.
## If a cache is available and the source matrix is no longer 
## identical to the original matrix a new cache is calculated 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i) && identical(x$src(), x$get())) {
                message("using cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}
