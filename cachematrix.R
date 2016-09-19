## Put comments here that give an overall description of what your
## functions do


## How I tested. I used one of the following methods. I copied either method into the RStudio 
## console window. 
##
## Method One  ###
##    cacheSolve(makeCacheMatrix(matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)))
##
## Method Two  ###
##    test_matrix <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)  #generates an inverseable matrix
##    inv_matrix <- solve(test_matrix)  #inverse the matrix from above
##    test_matrix   #print out the orginial matrix
##    inv_matrix    #print out the inverse so we can compare results to the 'cacheSolve' call.
##    cacheMatrix <- makeCacheMatrix(test_matrix)
##    cacheSolve(cacheMatrix)
##

## Write a short comment describing this function
## Takes a valid matrix. Valid means a matrix that can have an inverse.
## Note there is no error checking performed so an invalid matrix will throw 
## an error if the method 'cacheSolve' is called with the object return from this
## S3 class
makeCacheMatrix <- function(x = matrix()) {
  cacheMatrix <- NULL
  set <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) cacheMatrix <<- solve
  getinverse <- function() cacheMatrix
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## Write a short comment describing this function
## This is  an S3 object that takes as it's input parameter
## an object of type makeCacheMatrix. It will output the 
## inverse matrix of it's input matrix provide makeCacheMatrix
## contains a inverseable matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
