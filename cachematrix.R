## This function creates a special "matrix" object that can cache its inverse.
## First: Caching Inverse of a Matrix
## Second: Store Matrix, cache the inverse
## to test;
## trial_matrix <- makeCacheMatrix(matrix(1:12, 4, 2))
## then trial_matrix$get()

makeCacheMatrix <- function(x = matrix()) {
  var_inv <- NULL
  set <- function(y) {
    x <<- y
    var_inv <<- NULL
  }
  get <- function() x
  Set_Inverse <- function(inverse) var_inv <<- inverse
  Get_Inverse <- function() var_inv
  list(set = set,
       get = get,
       Set_Inverse = Set_Inverse,
       Get_Inverse = Get_Inverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## test if inverse was calculated and retrieve inverse from cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  var_inv <- x$Get_Inverse()
  if (!is.null(var_inv)) {
    message("getting cached data")
    return(var_inv)
  }
  mat <- x$get()
  var_inv <- solve(mat, ...)
  x$Set_Inverse(var_inv)
  var_inv
}