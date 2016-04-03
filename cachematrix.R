## Calculate the inverse of a Matrix. Retrieve the value
## out of Cache if value has already been calculated

## makeCacheMatrix compute a "special" Matrix object
## containing the inversed value

makeCacheMatrix <- function(x = matrix()) {    
    
    # Initialise inverse value to Null
    m <- NULL
    
    # set function: set matrix value
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # Get functio: get matrix vaule
    get <- function() x 
    
    # Set function: set matrix inverse value
    setinv <- function(inv) {
        m <<- inv
    }
    
    # Get function: Retrieve matrix inverse value
    getinv <- function() m 
    
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## CacheSolve compute the inverse of the matrix

cacheSolve <- function(x, ...) {
        
    ## Return a matrix that is the inverse of 'x'    
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # retrieve matrix data and compute inverse calculation
    data <- x$get()
    m <- solve(data, ...)
    
    # Set and return value
    x$setinv(m)    
    m
}
