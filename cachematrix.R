## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a matrix that is actually a list containing a
##  function to set the value of the matrix, get the value of this matrix,
##  set the value of the inverse matrix using solve, get the value of the
##  inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function utilizes the makeCacheMatrix function to get the inverse
## of the matrix. First it checks to see if the matrix inverse has already 
## been calculated, and if so it returns the cached matrix. If not it 
## follows the calculations in order to find the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
