## Matrix inversions can require a large amount of computation power, 
## so there may be some benefits to cacheing the inverse of a matrix
## rather than running the computation several times.
## I have used the following to functions to cache 
## the inverse of the matrix.

## Creating the "makeCacheMatrix" variable produces 
## a list that has an associated function that
## 1. set the value of the matrix
## 2. set the value of inverse of the matrix
## 3. get the value of the matrix
## 4. set the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function will also produce an inverse of the matrix.
## Running this function will first check that the inverse of the matrix 
## has been computed. If it has, it skips the computation and gets the result.
## If it hasn't been computed, it computes the inverse and sets the value in 
## the cache via the "setinverse" function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

x = rbind(c(1, -2/3), c(-2/3, 1))
m = makeCacheMatrix(x)
m$get()

## No cache in first round
cacheSolve(m)

## The second round retrieves from the cache.
cacheSolve(m)