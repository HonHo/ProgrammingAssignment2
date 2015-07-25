## The following functions store a given matrix and its inverse
## matrix in cache for reuse to improve peformance.

## This function takes an inversible matrix as argument and returns 
## a list of getter and setter functions. The getter functions are 
## used to access the matrix and its inverse matrix. The setter 
## functions are used to store the matrix and its inverse matrix
## in cache.

makeCacheMatrix <- function(x = matrix()) {
        matSolve <- NULL
        
        get <- function() {
                x
        }
        
        ## The input matrix could be different from
        ## the original one, so we need reset the 
        ## inverse of the matrix as well!
        set <- function(matIn) {
                x <<- matIn
                matSolve <<- NULL
        }
        
        getSolve <- function() {
                matSolve
        }
        
        setSolve <- function(matSolveIn) {
                matSolve <<- matSolveIn
        }
        
        ## Returns getter and setter functions as list
        list(get = get, set = set, 
                 getSolve = getSolve,
                 setSolve = setSolve)
}

## This function takes a list of getter and setter functions returned
## from makeCacheMatrix function as argument. It returns the inverse
## matrix from cache if it is already available. Otherwise it computes,
## stores, and returns the inverse of the matrix.

cacheSolve <- function(x, ...) {

        matSolve <- x$getSolve()
        
        ## The inverse found in the cache
        if(!is.null(matSolve)) {
                message("getting cached data...")
                return(matSolve)
        }
        
        ## Inverse not found, computes the inverse
        mat <- x$get()
        matSolve <- solve(mat, ...)
        
        ## Stores the inverse
        x$setSolve(matSolve)
        
        ## Returns the inverse
        matSolve
}

