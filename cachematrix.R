## These functions cache the inverse of a matrix in order
## to provide more efficient computing

## creates a matrix object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y){
      x <<- y
      im <<- NULL
  }
  get <- function() x
  setinv <- function(m) im <<- solve(m)
  getinv <- function() im
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## computes the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  im <- x$getinv()
  
  if(!is.null(im)){
    message("getting cached matrix")
    return(im)
  }
  data<- x$get()
  im <- solve(data, ...)
  x$setinv(im)
  im
  
}
