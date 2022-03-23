## Put comments here that give an overall description of what your
## functions do


## The following function "makeCacheMatrix" creates a special "matrix", which
## is a list containing a function to
##
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
			x <<- y
			m <<- NULL
	}
	get <- function() { x }
	setinvers <- function( solve ) { m <<- solve }
	getinvers <- function() { m }
	list( set = set, 
		  get = get,
		  setinvers = setinvers,
		  getinvers = getinvers )

}


## this function computes the inverse of the input matrix, based on the object created in makeCacheMatrix. 
## if the inverse has already been calculated, then this function should retrieve it from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
