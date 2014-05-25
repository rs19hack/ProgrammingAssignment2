## This pair of functions gives a computationally efficient way of cacluating the
## inverse of a matrix. It employs the lexical scoping feature of R. Effectively,
## this code reduces time complexity at the cost of space complexity.

## The computational efficiency comes from never calculating the inverse of
## a given input matrix more than once. This is achieved by caching the inverse of each
## new (distinct) input matrix as a list object. The cache is implemented
## outside the current environment using <<- (the superassignment operato)r.


# makeCacheMatrix creates a new list object that has the following methods:
# a getter, a setter, inverse calculator/setter, inverse retriever/getter.
# This list object is a wrapper around the input matrix 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y             # store the input matrix
    m <<- NULL          # placeholder for inverse matrix, to be filled with the setter
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


# cacheSolve checks if the given matrix has already been claculated in previous runs.
# If so, it retrieves the inverser matrix from the cache. If not, it calculates
# inverse using the "solve" library function, stores it in the cache and also 
# return the newly calculated inverse of the input matrix.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()  # check if already encountered this matrix
  if(!is.null(m)) {
    message ("getting cached inverse matrix")
    return(m)
  }
  message("Don't have computation cached. Calculating inverse of input matrix...")
  data <- x$get()   # pull out the actual input matrix from the input (list object)
  m <- solve(data)  # calculate the inverse of the given matrix
  x$setinverse(m)   # store the inverse in the cache
  m
}


# Test the code for functionality & correctness.
# a <- matrix(c(6, 9, 0, -9, 66, 2, -0.5, 25, 37), nrow= 3, ncol = 3)
# a[[2,2]] <- 0
# a
# b <- makeCacheMatrix(a)
# cacheSolve(b)
# a <- cacheSolve(b)
# b <- makeCacheMatrix(a)
# cacheSolve(b)
# round(cacheSolve(b), 3)
# a <- matrix(c(1,0,0,0,1,0,0,0,1), nrow = 3, ncol = 3)
# b <- makeCacheMatrix(a)
# cacheSolve(b)
