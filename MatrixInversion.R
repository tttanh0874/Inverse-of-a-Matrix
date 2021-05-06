## Catch the Inverse of a Matrix
## The aim of this assignment is to find the inverse of a square Matrix only.

## This function is used to create a matrix object that can cache its converse

makeCacheMatrix <- function(x = matrix()) {
 if (ncol(x)==nrow(x) && det(x)!=0) {
  i <- NULL
  set <- function(y) {
   x <<- y
   i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
 }
 else{
  return(message("No inverse matrix as the matrix must be square"))
 }
}

## This function is used to compute the inverse of the matrix 

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
 i <- x$getInverse()
 if (!is.null(i)) {
  message("getting cached data")
  return(i)
 }
 mat <- x$get()
 i <- solve(mat, ...)
 x$setInverse(i)
 i
}

##Testing the functions
##1. For the matrix(1:4,2,2)
##> a<- matrix(1:4,2,2)
##> b<-makeCacheMatrix(a)
##> cacheSolve(b)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

##2. For the matrix(1:9,3,3)
##> a<- matrix(1:9,3,3)
##> b<-makeCacheMatrix(a)
##No inverse matrix as the matrix must be square
