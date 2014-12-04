##  create a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the inverse of the "matrix" object
    i <- NULL
    
    ## set the value of the matrix 
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## get the value of the matrix 
    get <- function() x
    
    ## set the value of the inverse
    setInverse <- function(inverse) {
        i <<- inverse
    }
    
    ## get the value of the inverse 
    getInverse <- function() i
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ##  It first checks to see if the inverse has already been calculated.
    i <- x$getInverse()
    
    ## If so, it gets the inverse from the cache and skips the computation
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## Otherwise, it calculates the inverse of the data and sets the value of 
    ## the inverse in the cache via the setInverse function.
    data <- x$get()
    i <- solve(data) %*% data  
    x$setInverse(i)
    
    ## return the inverse
    i
}
