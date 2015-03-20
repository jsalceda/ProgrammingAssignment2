## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. The following pair of functions cache the inverse of a matrix.

## The first function, `makeVector` creates a special "vector", which is
## really a list containing a function to
## 1.  set the value of the matrix (set)
## 2.  get the value of the matrix (get)
## 3.  set the inverse of the matrix (setinvmat)
## 4.  get the inverse of the matrix (getinvmat)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmat <- function(invmat) m <<- invmat
  getinvmat <- function() m
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}

## The second function, `cacheSolve`computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse
## has already been calculated (and the matrix has not changed), 
## then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinvmat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvmat(m)
  m
}
