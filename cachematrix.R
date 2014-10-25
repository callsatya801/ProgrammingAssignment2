## Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
     # set invx (matrix inverse) to null
     invx <- NULL
     set <- function(y) {
          x <<- y
          invx <<- NULL
     }
     get <- function() x
     setMatrixInverse <- function(ix) invx <<- ix
     getMatrixInverse <- function() invx
     list(set = set, get = get,
          setMatrixInverse = setMatrixInverse,
          getMatrixInverse = getMatrixInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     invx <- x$getMatrixInverse()
     
     if(!is.null(invx)) {
          message("getting cached data")
          return(invx)
     }
     
     data <- x$get()
     invx <- solve(data)
     x$setMatrixInverse(invx)
     invx
}
