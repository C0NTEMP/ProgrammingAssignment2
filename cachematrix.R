# Put comments here that give an overall description of what your
## functions do

## This is a pair of functions of which caches the inverse of a matrix


## This function create a matrix which will cache the inverse of its inputs

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # Creates a NULL variable
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function()x # This will gets the value of the matrix
  setInv <- function(inv) i <<- inv # This is to set the value of the inverse, to be stored in i
  getInv <- function() i # Takes the value stored in i
  list(set = set, get = get, 
       setInv = setInv, 
       getInv = getInv)
}


## This function is used to retrieve the cached data from makeCacheMatrix
## cacheSolve will take an already calculated invserse from makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()
  if(!is.null(i)){
    message("getting cached data") # print message if i is not null
    return(i)
  }
  mtrx <- x$get()
  i <- solve(mtrx,...)
  x$setInv(i)
  i
}