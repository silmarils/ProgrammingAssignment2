## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

### assumption: the matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
  ## object has two properties
  ## - x is the actual matrix
  ## - m is its inverse (result of solve())
  ## 
  ## the 'first' time, the inverse is set to NULL
  ## each time the matrix is updated, the inverse is set to NULL
  ## inverse is calculated, only once, if matrix remains unchanted
  ## 
  ## -------------------------
  ## first time: 'object create ; inverse is NULL
  ## -------------------------
  m <- NULL
  ## -------------------------
  ## first function - set
  ## x ( from PARENT environment) is set to formal parameter
  ## m is set to NULL
  ## -------------------------
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## -------------------------
  ## second function - get
  ## simply return x 
  ## question: what happens if X is modified in parent, after this object is created ?
  ## -------------------------
  get <- function() x
  ## -------------------------
  ## third function - setmean
  ## simply sets m to the input parameter 
  ## Please note that the mean here is an arbitraty value (and could actually be wrong !!)
  ## -------------------------
  setinverse <- function(inverse) m <<- inverse
  ## -------------------------
  ## fouth function - getmean
  ## simply gets m from 'frame' (environment) 
  ## -------------------------
  getinverse <- function() m
  ## -------------------------
  ## last stement in function
  ## returns a list with 4 entry points 
  ## -------------------------
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
