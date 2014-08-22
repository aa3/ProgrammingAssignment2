## Programming Assignment 2: Lexical Scoping

## These two functions create a special "matrix" object, cache the inverse
## and retrieve the inverse if already cached and not changed.

## makeCacheMatrix() creates a special "matrix" object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {  ## input x is a matrix.
        s <- NULL                            ## s is the inverse.
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() {x}         ## function used by cacheSolve().
        setsolve <- function(solve)   ## function used by cacheSolve() and
                    {s <<- solve}     ## stores value using "<<-."
        getsolve <- function() {s}    
        list(set = set, get = get,    ## for accessing the sub-functions later.
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve() computes an inverse of the object returned by makeCacheMatrix().
## cacheSolve() retrieves the inverse from the cache if already computed.

cacheSolve <- function(x, ...) {     ## input is output from makeCacheMatrix().
        s <- x$getsolve()            ## acceses the answer object.
        if(!is.null(s)) {            ## checks if already cached.
                message("getting cached data")
                return(s)            ## if cached.
        }
        data <- x$get()              ## if not cached, access initial matrix.
        s <- solve(data, ...)        ## calculates the inverse.
        x$setsolve(s)                ## cache answer.
        s
}
