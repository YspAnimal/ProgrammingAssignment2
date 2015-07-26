##Sorry for my English!!!
##This functions compute and cache the inverse of a matrix.

## Function makeCacheMatrix return a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##              This list is used as the input to cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
  # cache to NULL
  invmatrix = NULL
  # create the matrix
  set = function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  # get the value of the matrix
  get = function() x
  # invert the matrix and place it in cache(invmarix)
  setinv = function(inverse) invmatrix <<- inverse 
  # get the inverted matrix from cache(invmarix)
  getinv = function() invmatrix
  # return the list of functions
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function return: inverse of the original matrix input to makeCacheMatrix()
cacheSolve <- function(x, ...) {
  invmatrix = x$getinv()
  # if the inverse has already calculated
  if (!is.null(invmatrix)){
    # get it from the cache. 
    message("getting cached data")
    return(invmatrix)
  }
  #calculates the inverse 
  mainmatrix = x$get()
  invmatrix = solve(mainmatrix, ...)
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(invmatrix)
  return(invmatrix)
}
