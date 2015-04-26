## Description of makeCachematrix:

## makeCacheMatrix is a function that reads out a list of functions that can be called as subfunctions to a separate function. 
## Each of the functions created (set, get, setsolve, getsolve) has a specific purpose:

## set called as a subfunction creates a new value of x (the original matrix) in the main function (parent environment), while setting the value of s (the inverse of the Matrix) to Null.
## get returns the value x that is stored in the main function.
## setsolve sets the value of s and stores that value in the main funtion.
## getsolve returns the value of s that is stored in the main function

## the list command takes all of these functions and creates a list of functions that is the output of makeCacheMatrix


makeCacheMatrix <- function(x = matrix()) {


	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}

	get <- function () x
	setsolve <- function(solve) s <<- solve
	getsolve <- function() s
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)

}


## Description of cacheSolve:

## cacheSolve is a function that takes as arguments the functions previously defined in makeCacheMatrix.
## its purpose is to see whether the inverse of the matrix x has already been calculated and stored as matrix s. 
## If so, it returns the value of s along with the message "getting cached data". It not, it calculates, stores, and returns a new value for s.

cacheSolve <- function(x, ...) {
        
	s <- x$getsolve()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)

	}

	data <- x$get()
	s <- solve(data, ...)
	x$setsolve(s)
	s

}







