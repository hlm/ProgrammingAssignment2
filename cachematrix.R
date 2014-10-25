## These functions can calculate the inverse of a matrix and cache
## the results; so it does not need to be recalculated when re-used.

## This function returns a closure that holds both the original
## matrix passed as argument x and its inverse
makeCacheMatrix <- function(x = matrix()) {
    # This variable holds the cached inverse of the matrix
    xi <- NULL

    ## Sets the enclosed matrix and resets any cached inverse
    set <- function(y) {
      x <<- y
      xi <<- NULL
    }

    ## Returns the enclosed matrix
    get <- function() x

    ## Caches the given solution to the inverse matrix
    setSolve <- function(solve) xi <<- solve

    ## Gets the cached inverse of the matrix
    getSolve <- function() xi

    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## This function expects a closure returned by makeCacheMatrix above
## and uses its caching capabilities to return the solution for
## the inverse matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xi <- x$getSolve()

    # Detects if a cached result was returned
    if(!is.null(xi)) {
      message("getting cached data")
      return(xi)
    }

    ## Gets the matrix...
    data <- x$get()

    ## ... and uses solve to compute the inverse matrix
    xi <- solve(data, ...)

    ## Stores the computed inverse in the cache
    x$setSolve(xi)

    ## Returns the inverse of the matrix
    xi
}

## Tests the functions
run.tests <- function() {
    message("Test #1: with a 2x2 matrix")
    m <- matrix(c(4, 2, 7, 6), 2, 2)

    # The expected inverse of the above matrix
    mi <- matrix(c(0.6, -0.2, -0.7, 0.4), 2, 2)

    # Initializes the closure with the above matrix
    cm <- makeCacheMatrix(m)
    print(cm$get())

    # Gets the inverse of the above function
    xi <- cacheSolve(cm)
    print(cm$getSolve())

    ## Calculates the inverse again, it should return the cached value
    xi <- cacheSolve(cm)

    ## Is this really the inverse matrix?
    ## This tests compares the inverse matrix returned with the one
    ## that was expected
    if(isTRUE(all.equal(xi, mi))) {
      message("Test #1 OK")
    } else {
      message("Test #1 FAIL")
    }

    # Test #2: With a 3x3 matrix
    message("Test #2: with a 3x3 matrix")
    m <- matrix(c(3, 2, 0, 0, 0, 1, 2, -2, 1), 3, 3)

    cm <- makeCacheMatrix(m)
    print(cm$get())

    ## Calculates the inverse matrix. It should not use the chache
    xi <- cacheSolve(cm)
    print(cm$getSolve())

    ## Once again requests the inverse, this time from the cache
    xi <- cacheSolve(cm)

    ## Is the returned matrix really the inverse?
    ## This actually tests if the matrix multiplied by its inverse
    ## is equal to a 3x3 identity matrix
    if(isTRUE(all.equal(m %*% xi, diag(3)))) {
      message("Test #2 OK")
    } else {
      message("Test #2 FAIL")
    }
}
