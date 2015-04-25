## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	## Initilize the result vector
	inv <- NULL  
	## Set the value of the Cache Matrix
	set <- function(y) {
                x <<- y
                inv <<- NULL
      }
	## Get the value of the Cache Matrix
      get <- function() x
      ## Set the inverse of the Cache Matrix
	setinverse <- function(inverse) inv <<- inverse
	## Get the inverse of the Cache Matrix
      getinverse <- function() inv
	## Store the functions into a list to be used easily
      list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	## Initilize the result
	inv <- x$getinverse()	
	## if there is a cached inverse, return it
	if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
	}
	## Otherwise, read x
	data <- x$get()
	## Take the inverse of x
	inv <- solve(data, ...)
	## Store the inverse 
	x$setinverse(inv)	
	## Return a matrix that is the inverse of 'x'
	inv
}