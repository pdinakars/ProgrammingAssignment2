## Inverse a given array and store it in a cache and
## if on executing find the inverse already present
## retrieve it from cache rather than performing the inverse
## operation

## makeCacheMatrix function has list of functions to 
## 1.set matrix 2. get matrix 3. set the inverse of matrix
## 4. get inverse of matrix
## Inverse of matrix is performed by ginv function hence used 
## MASS package

library('MASS')

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(ginv) m <<- ginv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## cacheSolve checks whether the matrix has been inverted already
## if done then provides it from cache rather then recalcualting.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- ginv(data)
  x$setinv(m)
  m
}