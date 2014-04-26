## functions written by Dave Martin, modelled after R.D. Peng's prototype
## 4/22/2014
## contact:  davekohlmartin@nospam-gmail.com
##
## Two functions, makeCacheMatrix() and cacheSolve() make special matrices and solves for its inverse
## if the inverse has already been solved, cacheSolve will retrieve it from cache instead of making 
## costly computation to solve it over again
##
## makeCacheMatrix is a function that will make a special "matrix", it returns a list of functions that
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the Inverse
## 4. get the value of the Inverse
## it does this by using the <<- operator which assigns values to the function's parent environment
## this has the effect of caching the result for later use

makeCacheMatrix <- function(x = matrix()) {
  ## defines four functions to set and get a matrix or its inverse to and from cache memory
  ## it uses the <<- operator to cache the matrix and its inverse in this function's parent environment
  ##
  ## Args:
  ##  x: the input matrix
  ##
  ## returns:
  ##   a list of functions set, get (the matrix), setInverse, getInverse (to set or get the Inverse)
  ##   from cache
  
  cm <- NULL ## cm is the free variable  used to cache the Inverse, set to NULL
  set <- function(y) { 
    ## this function will set or create the matrix
    x <<- y ## y is a matrix set it to store in x
    cm <<- NULL ## cm gets NULL'd because we just "made" a new matrix, whatever is in cm is no lnger the correct inverse matrix value
  }
  get <- function() x  ## get simply retrieves the matrix
  setInverse <- function(invMat) cm <<-invMat  ## setInverse takes the invMat (we assume this is the inverse) and stores it in the cache
  getInverse <- function() cm  ## this retrieves the inverse matrix
  
  ## we make a list of the above functions as the return value of makeCacheMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve will either solve for the inverse of a matrix or retrieve that inverse from
## cache to save from having to recompute the inverse of the input matrix

cacheSolve <- function(x, ...) {
  ## cacheSolve will either solve for the inverse of a matrix or retrieve that inverse from
  ## cache to save from having to recompute the inverse of the input matrix
  ##
  ##  Args:
  ##    x: is the matrix for which its inverse is to be solved
  ##
  ##  Returns:
  ##    a matrix that is the inverse of x
  
  cm <- x$getInverse()  #retrieve the inverse from cache
  if(!is.null(cm)) {
    ## if the result is not null, return it as the inverse, exit from the function
    message("getting cached data")
    return(cm)
  }
  ## cm is NULL, branches here
  data <- x$get()  ## grab the matrix
  cm <- solve(data, ...)  ## use solve to calculate its inverse
  x$setInverse(cm)  ## remember it in cache for future use
  cm  ## return the inverse
}