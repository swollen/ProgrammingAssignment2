## Two functions to create a global variable out of an inverted matrix.

## makeCacheMatrix creates a list of functions that can operate on 
## the input matrix, including creation of a global variable that 
## stores the solution to the inverse of the matrix. It is assumed
## that only invertible matrices are used.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)
}

## cacheSolve creates the inverse of a matrix provided with the 
## functions output by the function makeCacheMatrix.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
	m <- x$getsolve()
	if(!is.null(m) {
		message("getting cached data")
		return(m)
	}
	data <- x$get
	m <- solve(data, ...)
	x$setsolve(m)
	m
}
