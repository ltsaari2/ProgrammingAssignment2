## These functions create a matrix object that can be used to create,save and return a matrix and its inverse to the cache memory

## Function to make create a "special" matrix object that can cache its inverse to memory.
## makeCacheMatrix function creates a list of functions that are used to set and return the matrix and its inverse
## function also tests if the matrix can be inverted and gives an error message if not
makeCacheMatrix <- function(m = matrix()) {
  #Function to check if m is invertible
  f <- function(m) class(try(solve(m),silent=T))=="matrix"
  #Check if matrix is invertable and end the function with error message if it is not, otherwise continue
  if (!f(m)) {
    return("Matrix not invertable")
  }
  
  #set the matrix and inverse
  minv <- NULL
  set <- function(y) {
    m <<- y
    minv <<- NULL
  }
  #get the matrix
  get <- function() m
  #set the inverse matrix
  setinverse <- function(inv) minv <<- inv
  #get the inverse matrix
  getinverse <- function() minv
  #Build the special matrix object with its inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function takes a matrix object created by the makeCacheMatrix function as an argument
## the function uses solve function to calculate and return the inverse of the matrix given in the argument
## before calculation of the inverse the function checks if the a cached inverse already exists and returns it

cacheSolve <- function(m, ...) {
  #get cached inverse matrix if it is available
  minv <- m$getinverse()
  #check if the cached inverse was available and return it
  if (!is.null(minv)) {
    message("getting the cached inverse")
    return(minv)
  }
  #calculate inverse and cache it
  data <- m$get()
  minv <- solve(data)
  m$setinverse(minv)
  minv
  
}
