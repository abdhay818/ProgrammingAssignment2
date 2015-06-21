## Caching the Inverse of a Matrix 
##
## There are 2 functions in this file : 
## (1) makeCacheMatrix
## (2) cacheSolve
## 
## These 2 functions are used to cache potentially time-consuming 
## computations of matrix inversion.


## Function Name: makeCacheMatrix()
## Descriptions:  This function creates a special "matrix" object that can cache its matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  myInversedMatrix <- NULL #initialized the inverse matrix variable, myInversedMatrix, with NULL
  
  # set the value of the matrix
  set <- function(y){
    x <<- y
    myInversedMatrix <<- NULL    
  }
  
  # get the value of the matrix
  get <- function() { 
    x 
  }
  
  # set the value of the matrix inverse
  setInverve <- function(inverseValue) { 
    myInversedMatrix <<- inverseValue 
  }
  
  # get the value of the matrix inverse
  getInverse <- function() { 
    myInversedMatrix # return the inversed matrix
  }
  
  # create a list object for all functions 
  list(set = set, get = get,
       setInverve = setInverve,
       getInverse = getInverse)

}


## Function Name: cacheSolve()
## Descriptions:  This function calculates the matrix inverse created with makeCacheMatrix() function. It
##                checks to see if the inverse has already been calculated. If so, it gets the inverse value 
##                from the cache and skips the time-consuming computation. Otherwise, it calculates the matrix 
##                inverse and sets the value of the inverse in the cache via the setInverse() function.

cacheSolve <- function(x, ...) {
  
  # get the inverse value
  inversedMatrix <- x$getInverse() 
  
  # check to see if the inverse already in cache memory
  if (!is.null(inversedMatrix)){
    message("Getting cached data")
    return inversedMatrix  # return the inverse value from cache    
  }
  
  ## Else, the inverse value is not in the cache, we need to compute the matrix's inverse
  data <- x$get()
  inversedMatrix <- solve(x)   # compute the matrix inversion using R solve() function
  x$setInverve(inversedMatrix) # set the matrix inverse value into cache
  
  inversedMatrix  ## Return a matrix that is the inverse of 'x'
}
