## The functions in this file use the concept of lexical scoping to cache the
## inverse of a square invertible 
## matrix for later use to reduce computation time in the case
## of multiple uses of the same inverse.

## makeCacheMatrix creates a object containing a matrix and the ability to cache
## it's inverse. A list is created as part of the object to aid retrieval.

makeCacheMatrix <- function(x = matrix()) {
  ## @x: a square invertible matrix
  ## return: a list containing functions to
  ##             set the matrix (set)
  ##             get the matrix (get)
  ##             set the inverse (setinverse)
  ##             get the inverse (getinvers)
  ##         this list is used as the input to cacheSolve()
  
  inv = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinverse = function(inverse) inv <<- inverse 
  getinverse = function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve computes the inverse of a square invertible matrix object returned
## by makeCacheMatrix(). If the inverse has been calculated previously it returns
## a cached value instead of recomputing.

cacheSolve <- function(x, ...) {
   ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat = x$get()
  inv = solve(mat, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}
