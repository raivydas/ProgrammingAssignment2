## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Caches the inverse of the matrix x
makeCacheMatrix <- function(x = matrix()) {
    ## Sets the inverse inv of the matrix x to NULL
    inv <- NULL
    ## Sets x to y if y is a new matrix. Sets inv to NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## Returns the matrix x
    get <- function() x
    ## Sets the inverse inv of the matrix x to inverse
    setinverse <- function(inverse) inv <<- inverse
    ## Returns the inverse inv of the matrix x
    getinverse <- function() inv
    ## Returns the list with the cached data
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function

## Computes the inverse of the matrix x. If the inverse has been computed, returns the cached inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    ## If the inverse inv has been cached, return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## If the inverse has not been cached, compute and store it
    X <- x$get()
    inv <- solve(X)
    x$setinverse(inv)
    inv
}
