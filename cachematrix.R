## Purpose of file:
## This file contains two functions makeCacheMatrix and cacheSolve.
## Computing inverse of a matrix is costly operation. 
## Functions, makeCacheMatrix and cacheSolve, allow computing and caching of inverse of matrix.

## Function makeCacheMatrix creates specialized wrapper in form of a list 
## based of provided matrix that can encapsulate matrix and its inverse.

makeCacheMatrix <- function(m = matrix()) {
	mInv <- NULL

	## Setter function for m
	set <- function(newM) {
		m <<- newM
		## Since m has been updated. Lets purge this inverse.
		##
		## Note:
		## This can be optimized by comparing newM to m, if newM and m are same in terms of
		## elements, inverse will not change. hence we can keep the inverse. 
		## However, for the purpose of completing this exercise it is unnecessary.
		mInv <<- NULL
	}

	## Returns current m
	get <- function() {
		m
	}

	## Setter function for mInv. Sets inverse of m to given value
	setInverse <- function(newMInv) {
		mInv <<- newMInv
	}

	## Returns current stored inverse of matrix
	## Note: This function would return NULL, in case set function has been called 
	## and as a following call setInverse was not called to set inverse again.
	getInverse <- function() {
		mInv
	}

	## Lets return this special list which provides functions to get/set matrix and its inverse
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Function cacheSolve computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	## Lets see if inverse is already set
	mInv <- x$getInverse()

	## Validate
	if(!is.null(mInv)) {
		## Indicate that cached inverse is being returned
		message("returning cached inverse")
		return(mInv)
	}

	## Cached inverse does not exit, lets calculate and set
	x$setInverse(solve(x$get(), ...))
	## Inverse is set, lets get and return
	x$getInverse()
}

## 
## Instructions and sample runs:
## 
## > x <- makeCacheMatrix()
## > cacheSolve(x)
##      [,1]
## [1,]   NA
## >
## >
## > cacheSolve(x)
## returning cached inverse
##     [,1]
## [1,]   NA
##
## > cacheSolve(makeCacheMatrix())
##     [,1]
## [1,]   NA
## >
## >
## > y <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
## > 
## > 
## > y$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > 
## > 
## > 
## > cacheSolve(y)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > 
## > cacheSolve(y)
## returning cached inverse
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > 
## > 
## > y$set(matrix(4:1, nrow = 2, ncol = 2))
## > 
## > 
## > y$get()
##      [,1] [,2]
## [1,]    4    2
## [2,]    3    1
## > 
## > cacheSolve(y)
##      [,1] [,2]
## [1,] -0.5    1
## [2,]  1.5   -2
## > 
## > 
## > cacheSolve(y)
## returning cached inverse
##      [,1] [,2]
## [1,] -0.5    1
## [2,]  1.5   -2
## > 
## > 
