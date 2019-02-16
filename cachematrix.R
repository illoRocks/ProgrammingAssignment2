## These functions calculate and cache the inverse of a matrix. 

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  # check if matrix is squared
  stopifnot(nrow(x) == ncol(x))
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) m <<- inverse
  get_inverse <- function() m
  
  # return list
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix and cache them.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
}

# Example
# x <- makeCacheMatrix(matrix(sample.int(9), 3))
# cacheSolve(x)

