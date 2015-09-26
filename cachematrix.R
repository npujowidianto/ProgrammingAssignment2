## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function is to calculate the inverse of the given matrix
## Assume that the matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
  ## initialize the value as null
  inv_matrix <- NULL
  
  ## to get the matrix
  get_matrix <- function() x
  
  ## to set the matrix
  set_matrix <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  
  ## to set the inverse of the matrix manually
  set_inverse_matrix <- function(solve) inv_matrix <<- solve
  
  ## to get the last computed inverse of the matrix
  get_inverse_matrix <- function() inv_matrix
  
  ## to combine all the functions
  list (set_matrix = set_matrix, 
        get_matrix = get_matrix, 
        set_inverse_matrix = set_inverse_matrix, 
        get_inverse_matrix = get_inverse_matrix)
}


## Write a short comment describing this function
## This function is to check whether the inverse of the matrix still needs to be re-computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## check the current value of the inverse matrix
  inv_matrix <- x$get_inverse_matrix()
  
  ## if the inverse of matrix has been computed (not null), then re-use the cached data
  if(!is.null(inv_matrix)) {
    message ("getting cached data")
    return(inv_matrix)
  }
  
  ## to get the matrix
  data <- x$get_matrix()
  
  ## to compute the inverse of the matrix
  inv_matrix <- solve(data)
  
  ## to set the value of the inverse of the matrix
  x$set_inverse_matrix(inv_matrix)
  
  ## to display the value of the inverse of the matrix
  inv_matrix
}
