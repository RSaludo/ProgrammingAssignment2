## This is the 2nd attempt at this assignment branched to Feb22-ProgrammingAssignment2.
## Line 9 starts the makeCacheMatrix function that stores the output of the cacheSolve
## function frim the variable 'i'.
## Line 26 starts the cachSolve function that provides the inverse of the given matrix (x).

## I changed the variables from the original mean cache to solve and substituted 'i' for 'm'.
## The vector container is created first. The assignment opperator '<-' was used in lieu of '='.

makeCacheMatrix <- function(x <- matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) i <<- solve
        getsolve <- function () i
        list(set <- set, get <- get,
             setSolve <- setSolve,
             getSolve <- getSolve)
}

## Similar to above, I used solve in lieu of mean and '<-' in lieu of '='.
## This function will return a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {
        i <- x$getSolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setSolve(i)
        i
}