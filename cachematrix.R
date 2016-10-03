## Programming Assignment 2: Lexical Scoping
## Brandon Share

## The following functions will allow a user to cache the inverse
## of a Matrix

## this function creates a special "matrix" object that can cache its inverse
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


# The following function returns the inverse of the matrix.
# If the inverse has already been computed it gets the cached
# results, otherwise it computes the inverse and stores the results.

# This function assumes that the matrix is always invertible
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

