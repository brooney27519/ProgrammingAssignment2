## These functions allow a user to get the inverse of a given matrix that is stored in cache memory
## If the inverse of the matrix is not in cache, the inverse of the matrix will be calucalted and returned as well

## makeCacheMatrix does the following:
## (1) intitializes the inverse matrix object, inv_m, to NULL
## (2) creates a function to store (set) the matrix given by x in cache
## (3) creates a function to get the matrix values from cache
## (4) creates a function to store (set) the calculated inverse in cache
## (5) creates a function to get the value of the inverse matrix from cache
## (6) returns a list of these functions and makes them available to the cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_m <<- inverse
  getinverse <- function() inv_m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve attempts to get the inverse matrix of the matrix given by x
## If the inverse matrix is in cache, it is returned
## if the inverse matirx is not in cache, the inverse is calculated and stored in cache
cacheSolve <- function(x, ...) {
  inv_m <- x$getinverse()
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  data <- x$get()
  inv_m <- solve(data, ...)
  x$setinverse(inv_m)
  inv_m
}