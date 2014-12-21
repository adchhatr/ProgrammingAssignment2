## makeCacheMatrix :This function creates a special "matrix" object that can cache its inverse.
##                  It uses following functions.
##                  1. setmatrix : to set the value of the matrix
##                  2. getmatrix : to get the value of the matrix
##                  3. setinverse: to set the value of the matrix inverse
##                  4. getinverse: to set the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setmatrix <- function(y) {
              x <<- y         
              i <<- NULL
  }
  getmatrix <- function()         {x}
  setinverse <- function(inverse) {i <<- inverse}
  getinverse <- function()        { i }
  
  list(setmatrix = setmatrix, getmatrix = getmatrix,setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
##             makeCacheMatrix above.If the inverse has already been calculated 
##             (and the matrix has not changed), then the cachesolve should retrieve 
##             the inverse from the cache.


cachesolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
            message("getting cached data")
            return(i)
  }
  data <- x$getmatrix()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}