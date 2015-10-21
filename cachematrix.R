## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Matrix inversion is a costly computation if computing it repeatedly.
# Caching the inverse of the matrix after first computation is quite
# beneficial to avoid repeatedly compute the matrix inversion

# Assuming all the matrices given is invertible

# The makeCacheMatrix function contains a list of functions in order to
# set and get the value of matrix, and also set and get the value
# of inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  # Setting a null variable
  inverse <- NULL
  
  # The set function in which to set the value of matrix
  set <- function(y){
    
    # Assign y into x
    x <<- y
    
    # Setting a null variable
    inverse <- NULL
  }
  # The get function in which to get the value of matrix
  get <- function() x
  
  # The setInvMatrix function in which to set the value of inverse matrix
  setInvMatrix <- function(solve) inverse <<- solve
  
  # The getInvMatrix function in which to get the value of inverse matrix
  getInvMatrix <- function() inverse
  
  # Return a vector which containing all the defined functions
  list(set = set, get = get, 
       setInvMatrix = setInvMatrix, 
       getInvMatrix = getInvMatrix)
}


## Write a short comment describing this function

# The cacheSolve function will return the value of inverse matrix. However,
# it will first to check whether the inverse of the matrix has been 
# computed via a conditional loop. If yes, it will skip the computation 
# and return the value. Or else, it will compute the inverse of the matrix 
# and set the value into the cache via setInvMatrix function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Get the value of inverse matrix
  inverse <- x$getInvMatrix()
  
  # A conditional loop to check whether the value of inverse 
  # matrix has been already computed
  if(!is.null(inverse)){
    
    # Return a message if the condition is true
    message("Getting cached data")
    
    # Return the value of inverse matrix and skip the computation 
    # if the condition is true
    return(inverse)
  }
  
  # Get the value of matrix 
  data <- x$get()
  
  # Calculate the inverse of the given matrix
  inverse <- solve(data, ...)
  
  # Set the value of inverse matrix into the cache with the respective matrix
  x$setInvMatrix(inverse)
  
  # Return the value of inverse matrix
  inverse
}
