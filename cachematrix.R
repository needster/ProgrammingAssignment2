## These functions may be used to compute, cache, and return 
## the inverse of a matrix. The follwing is an example of
## how to call these functions in the console after sourcing the file. 
## This example assumes that the value of 'testMatrix' is an invertible matrix.
## In this example testMatrix =      
##              a b
##      [1,]    4 3
##      [2,]    1 1
## 
## execute the following commands in the console
## > result = makeCacheMatrix(testMatrix)
## > cacheSolve(result)
##      
## output will be:
##          [,1] [,2]
##      a    1   -3
##      b   -1    4
##   
## now execute the following command:
## > cacheSolve(result)
##
## output will be:
##  getting cached data
##          [,1] [,2]
##      a    1   -3
##      b   -1    4



## The makeCacheMatrix function creates a special "matrix" object that 
## can cache its inverse and assumes the incoming matrix is always 
## invertible.
makeCacheMatrix <- function(incomingMatrix = matrix()) {
    
    inverseMatrix <- NULL
    
    ## function that sets the incoming matrix
    set <- function(matrixToSet) {
        incomingMatrix <<- matrixToSet
        inverseMatrix <<- NULL
    }
    
    ## function that gets the matrix
    get <- function() {
        incomingMatrix
    }
    
    ## function that sets the inverse matrix
    setinverse <- function(inverseToSet) {
        inverseMatrix <<- inverseToSet
    }
    
    ## function that gets the inverse matrix
    getinverse <- function() {
        inverseMatrix
    }
    
    ## create a list that maps function names to functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Thee cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then 
## cachesolve retrieves the inverse from the cache.
cacheSolve <- function(matrixToInvert, ...) {
    
    ## call the getinverse function
    inverseMatrix <- matrixToInvert$getinverse()
    
    ## if inverseMatrix is not NULL, 
    ## it was retrieved from the cache
    ## print a message and return the inverse
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
        
    ## otherwise, we need to solve for the inverse,
    data <- matrixToInvert$get()
    inverseMatrix <- solve(data)
    
    ## cache the inverse and return it
    matrixToInvert$setinverse(inverseMatrix)
    inverseMatrix
}
