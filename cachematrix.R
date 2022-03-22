## Put comments here that give an overall description of what your
## functions do

## The function first initializes the set, get, set_inverse and get_inverse methods.
makeCacheMatrix <- function(x = matrix()) {
  
  ## function to check if matrix x is "square and invertible" 
  check <- function(x){ 
      if(nrow(x) != ncol(x)) {
         stop("matrix must be square")
      }
  
      if(det(x) == 0) {
         stop("matrix is not invertible")
      }
  }
  
  inv <- NULL
  set <- function(y){
    check(y)
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {x}
  set_inverse <- function(inversed) { inv <<- inversed}
  get_inverse <- function() {inv}
  
  list( set = set,
        get = get,
        set_inverse = set_inverse,
        get_inverse = get_inverse)

}


## This function solves for the inverse of the matrix and caches it in "inv"
cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  
  if(!is.null(inv)) {
    message("getting cached data....")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat)
  set_inverse <- x$set_inverse(inv)
  
  return(inv)
}
