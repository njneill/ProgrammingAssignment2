## This pair of functions can be used to calculate the inverse of a matrix and
## cache the result so that it does not need to be repeatedly calculated

## This function "vectorizes" the matrix into a list of functions that can 
## be used to cache and retrieve the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinv <- function (inv) i <<- inv
	getinv <- function() i
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function first checks to see if the inverse of the matrix is already 
## cached. If it is, it displays the cached inverse; if not, it calculates
## the inverse and caches it

cacheSolve <- function(x, ...) {
	i <- x$getinv()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data,...)
	x$setinv(i)
	i
}
