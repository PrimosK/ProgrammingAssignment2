## Assignment: Caching the Inverse of a Matrix

## A function that returns a list containing functions to: 
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the matrix's inverse
## - get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  # set new matrix and clear the inverse
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # return the matrix
  get <- function() x
  
  # set the inverse
  setinverse <- function(inverse) i <<- inverse
  
  # return the inverse
  getinverse <- function() i
  
  #Expose the methods
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculates the inverse of matrix using the above function

cacheSolve <- function(x, ...) {
  
  # get inverse
  i <- x$getinverse()
  
  #check if inverse already calculated
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  
  # calculate the inverse
  i <- solve(data, ...)
  
  #set inverse
  x$setinverse(i)
  
  i
}
