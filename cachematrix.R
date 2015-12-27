## Functions to cache the result of a matrix operation
## and a function to calculate inverse of the matrix and cache the result.
##
## A quick test for the functionality is
## zm <- makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(zm)
##
## should get the results
##	-2, -1.5
##   1, -0.5
##
## A second run should show the message "getting cached data"
##

## Generalized class? to store a matrix and cache a results

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setresult <- function(matrix) m <<- matrix
  getresult <- function() m
  list(set = set, get = get,
       setresult = setresult,
       getresult = getresult)

}


## Calculate the inverse of the a matrix.  
## Using the CacheMatrix to store the inverse after solving
## Future calls will return the cached value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getresult()
	if( !is.null(inv)){
    	message("getting cached data")
    	return(inv)
	}
	data <- x$get()
	inv <- solve(data,...)
	x$setresult(inv)
	inv
}
