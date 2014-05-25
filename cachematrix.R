## Put comments here that give an overall description of what your
## functions do

## WThis function will store the cached matrix and will look up when necessary.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## Create function to pull data
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## creates function get for pulling, setinv for saving the inverse, 
    ## getinv for obtaining the inverse once its cached.
    
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function will create the look for a cached inverse of matrix x if not it will create and cached it.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ## looks for cached data for the matrix if not then create the inverse
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## creates the inverse and save it
    
    data <- x$get()
    m <- solve(data,...)
    x$setinv(m)
    m
}
