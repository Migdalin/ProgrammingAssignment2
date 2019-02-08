## To avoid performing the expensive Inverse Matrix operation
## more than once, we create a pseudo-object inside a list (sic)
## that stores a matrix and its inverse.

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(theMatrix = matrix()) {
    theInverse <- NULL

    getMatrix <- function() {
        theMatrix
    }
    setMatrix <- function(iMatrix) {
        theMatrix <<- iMatrix
        theInverse <<- NULL
    }
    
    getInverse <- function() {
        theInverse
    }
    setInverse <- function(iInverted) {
        theInverse <<- iInverted
    }
    
    list(getMatrix = getMatrix,
         setMatrix = setMatrix, 
         getInverse = getInverse,
         setInverse = setInverse)
}


## Computes the inverse of a matrix, if it has changed, or
## retrieves the inverse from cache, if it hasn't changed.
cacheSolve <- function(iMatrixCache, ...) {
    inverted <- iMatrixCache$getInverse()
    if(!is.null(inverted)) {
        return(inverted)
    }
    
    inverted <- solve(iMatrixCache$getMatrix())
    iMatrixCache$setInverse(inverted)
    return(inverted)
}


test_CanInvert3x3 <- function() {
    matrixA <- matrix(runif(9), 3, 3)
    matrixB <- solve(matrixA)

    theCache <- makeCacheMatrix(matrixA)
    solved <- cacheSolve(theCache)
    stopifnot(identical(solved, matrixB))
    
    solved <- cacheSolve(theCache)
    stopifnot(identical(solved, matrixB))
    
    matrixA <- matrix(runif(9), 3, 3)
    matrixB <- solve(matrixA)
    
    theCache$setMatrix(matrixA)
    solved <- cacheSolve(theCache)
    stopifnot(identical(solved, matrixB))
    
    invisible(NA)
}

