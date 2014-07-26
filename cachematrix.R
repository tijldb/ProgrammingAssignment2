## Put comments here that give an overall description of what your
## functions do
## 
## makeCacheMatrix creates an object with two members (x: matrix and m: its inverse)
## cacheSolve returns the inverse of the matrix (m) possibly after calculating it if not present in the cache

## the class has two members: x & m
## x being the matric
## m being its inverse
## each have a get and set function in the class

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) m <<- inv
	getinverse <- function() {
		m
	}
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## the function retrieves the cached inverse (which might be NULL)
## it checks whether it is null, if not it returns the cached version
## if so, it retrieves the data and calculates the inverse, stores it in the cacheSolve
## and returns the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
		if (!is.null(m)) {
			message("getting cached data")
			return(m)
		}
		data <- x$get()
		m <- solve(data)
		x$setinverse(m)
		m
}
