## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function makeCacheMatrix does four things: sets the value
## of the matrix, gets the value of the matrix, sets the value of 
## the inverse, and lastly gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  nv <- NULL
  set <- function(y) {
    x <<- y
    nv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) nv <<- inverse
  getinverse <- function() nv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function
## The function cacheSolve caculates the inverse of the matrix created
## in the above function. However, if the inverse has already been calculated
## then it gets the inverse from the cache and skips the computation.
## Otherwise, it caculates the inverse of the data and sets the value
## of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  nv <- x$getinverse()
  if(!is.null(nv)) {
    message("getting cached data")
    return(nv)
  }
  data <- x$get()
  nv <- solve(data)
  x$setinverse(nv)
  nv
}
