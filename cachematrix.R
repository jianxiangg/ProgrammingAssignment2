## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## The two functions below can store a matrix and cache its inverse. 

## This function can cache and retrieve a matrix, x

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	get <- function() x
	setInv <- function(inverse) inv <<- inverse
	getInv <- function() inv
	list(set = set, get = get, setInv = setInv,
		getInv = getInv)
}


## checks if cached data of inverse matrix is available.
## If true, returns said cached data. Else, calculates it and cache results

cacheSolve <- function(x, ...) {
	inv <- x$getInv()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setInv(inv)
	inv
}
