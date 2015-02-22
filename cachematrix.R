## Two functions are in this code.
## The first function 'makeCacheMatrix' is used to create a square matrix.
## The second function is used to cache the inverse of the matrix.


## The following function generates an invertible matrix.
## This function also sets and gets the value of the matrix.
## Additionally, this function also sets and gets the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function "cacheSolve" is used to check if the inverse of the matrix is available in the cache.
## If available in the cache, the result is returned from the cache.
## If not available in the cache, the inverse is computed, cached in the memory and retuned to the console.
## The function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
            message(" Matrix inverse from the cache.")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}


#Sample program run and the output

# x = rbind(c(5, -20), c(-20, 5))
# m = makeCacheMatrix(x)
# m$get()
#       [,1] [,2]
# [1,]    10   5
# [2,]    20   10

# cacheSolve(m)
            # [,1]        [,2]
# [1,] -0.01333333 -0.05333333
# [2,] -0.05333333 -0.01333333

# cacheSolve(m)
# Matrix inverse from the cache.
            # [,1]        [,2]
# [1,] -0.01333333 -0.05333333
# [2,] -0.05333333 -0.01333333
