# In the makeCacheMatrix function, I list the 4 
# required functions for cacheSolve to work
# To test the function, try the following for example:
# a <- makeCacheMatrix(matrix(1:4,2,2))
# a$get() will give you the matrix, a$getInverse will return a NULL by default

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# The cacheSolve() functions first checks if an inverse has already
# been calculated for x. If the answer is yes, it returns the inverse.
# If the answer is no, it calculates the inverse.
# To try it, after calling the makeCacheMatrix() function, do the following:
# cacheSolve(a) - this should return the inverse, without the "Getting cached data"
# for the first time. Calling it the second time will get it from cache, showing
# you the "Getting cached data" text also. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("Getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}