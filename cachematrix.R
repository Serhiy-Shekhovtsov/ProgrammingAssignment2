## this file contains two functions that demonstrate an example of 
## handling time consuming opertion using caching
## function makeCacheMatrix creates enhanced matrix object
## with methods to get and set Matrix and to get and set Inverse of a Matrix

## creates a list with for methods:
##   get - returns a matrix object
##   set - sets a matrix object
##   getInverse - returns a cached inversed matrix
##   setInverse - sets cached inversed matrix
makeCacheMatrix <- function(mtx = matrix()) {
    
    ## inverse of the matrix
    inversed <- NULL
        
    ## sets a new matrix and clears cached value
    set <- function(newMatrix) {
        mtx <<- newMatrix
        inversed <<- NULL
    }
    
    ## returns current matrix
    get <- function() mtx
    
    ## caches inverse of the matrix
    setInverse <- function(newInversed) inversed <<- newInversed
    
    ## returns current inverse of the matrix
    getInverse <- function() inversed
    
    ## returns a list with all functions defined above
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix function
## if the inverse has already been calculated, then the cachSsolve should retrieve 
## the inverse from the cache, else it should calculate the inverse and cache it
cacheSolve <- function(matrix, ...) {
    
    ## get cached value
    invMatrix <- matrix$getInverse()
    
    ## if cached value exist - then return it
    if(!is.null(invMatrix)) {
        message("returning cached data")
        return(invMatrix)
    }
    
    ## get original matrix object
    originalMatrix <- matrix$get()
    
    ## calculate inversed matrix
    invMatrix <- solve(originalMatrix, ...)
    
    ## cache inversed matrix
    matrix$setInverse(invMatrix)
    
    ## return calculated value
    invMatrix
}
