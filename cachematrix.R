## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y){
                    x <<- y
                    inv <<- NULL
}
        get <- function()x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function()inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

## Following function computes the inverse of the matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
                message ("Getting Cached Data")
                return(inv)
                }
        matrx <- x$get()
        inv <- solve(matrx,...)
        x$setinverse(inv)
        inv
        
}
