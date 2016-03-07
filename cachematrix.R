## The following functions cache the inverse of a matrix
## 

## makeCacheMAtrix creates a special "matrix" object that can cache
## its inverse.It is a list containing a function to
## 1) Set the value of the vector
## 2) Get the value of the vector
## 3) Set the value of the inverse
## 4) Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

	matrixInverse <- NULL

	set <- function(y) {	
		x <<- y
		matrixInverse <<- NULL
	}

	get <- function() x

	setInverse <- function (mInverse) matrixInverse <<- mInverse

	getInverse <- function() matrixInverse

	list(set = set, get = get,
	     setInverse = setInverse,
	     getInverse = getInverse)

}


## cacheSolve calculates the inverse of special "matrix"
## object created with above function. It first checks if
## inverse is calculated. If so, it gets the inverse from
## the cache and skips computation. Otherwise, it calculates
## the inverse of the "matrix" and sets the va;ue of the inverse
## in the cache via the setInverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	matrixInverse <- x$getInverse()
	if(!is.null(matrixInverse)) {
		message("getting cached data")
		return(matrixInverse)
	}

	data <- x$get()
	matrixInverse <- solve(data, ...)
	x$setInverse(matrixInverse)
	matrixInverse
}
