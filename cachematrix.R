## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message ("getting cached inverse matrix")
    return(m)
  }
  message("Don't have computation cached. Calculating inverse of input matrix...")
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

a <- matrix(c(1:9), nrow= 3, ncol = 3)
a[[2,2]] <- 0
a
b <- makeCacheMatrix(a)
cacheSolve(b)

