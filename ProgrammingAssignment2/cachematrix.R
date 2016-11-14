## The two functions created below creates an object that stores a matrix 
## and caches its inverse.  These functions saves you from the need of
## repeated computations.

## This function creates an object "matrix" that can cahe its inverse

makeCacheMatrix <- function(x = matrix()) {
			inv <- NULL
			set <- function(y) {
				x <<- y
				inv <<- NULL
			}
			get <- function() x
			setInverse <- function(inverse) inv <<- inverse
			getInverse <- function() inv
			list(set = set,
				get = get,
				setInverse = setInverse,
				getInverse = getInverse)

}


## The function below computes the inverse of the object "matrix" created
## by makeCacheMatrix above.  If the inverse has been calculated and the 
## matrix has not changed, then it should retrieve the inverse from the cache.

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
