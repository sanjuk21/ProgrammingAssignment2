## Put comments here that give an overall description of what your
## functions do
## Creating two functions, one to return a special vector and the other to calcu-
## late the inverse of a matrix

## Write a short comment describing this function
## The following function makeCacheMatrix returns a list containing functions to
## set the value of the matrix, get the value of the matrix, set the inverse of
## the matrix and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<-y
		m <<- NULL
	}
	get <- function() {
		x
	}
	setinverse <- function(inverse) {
		inv <<- inverse	
	}
	getinverse <- function(){
		inv
	}
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function calculates the inverse of the matrix created with the function
## above. If the inverse already exists the cached value is returned. If it's 
## null then the inverse is calculated and then returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting matrix inverse that's already cached  in memory ...")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data,...)
	x$setinverse(inv)
	inv
}