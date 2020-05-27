## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## creats cache matrix object

makeCacheMatrix <- function(x = matrix()) {
  m <- matrix()
  p <- matrix()
  set <- function(y = matrix()) {
    if (as.vector(is.na(y[1,1]))){
      stop('Empty matrix')
    }
    x <<- y
    m <<- matrix()
  }
  get <- function() x
  setinverse <- function(inver) m <<- inver
  getinverse <- function() m
  prev <- function(pre) p <<- pre
  getprev <- function() p
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse,
       prev = prev,
       getprev = getprev)
}


## Write a short comment describing this function
## inverse a matrix to set it to cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if((!as.vector(is.na(m[1,1]))) & identical(x$getprev(),x$get())) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  id <- diag(dim(data)[1])
  m <- solve(data,id)
  x$setinverse(m)
  x$prev(data)
  m
}
