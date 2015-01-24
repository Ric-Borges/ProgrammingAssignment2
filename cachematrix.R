## Put comments here that give an overall description of what your
## functions do

## Similarly to the example provided in the instructions
## makeCacheMatrix creates a list with a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the valude of the inverse of the matrix, using the R function solve()
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
	x <<- y
	inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set=set, get=get, 
		setinverse=setinverse, 
		getinverse=getinverse)
}


## this function returns the matrix inverse. 
## If the inverse has been computed it returns the cached value
## If the inverse has not been computed, it computes it for the first time and puts it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cacheSolve <- function(x, ...) {
	m <- x$getinv()
	if(!is.null(m)) {
		message("getting cached data.")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m
}
}
