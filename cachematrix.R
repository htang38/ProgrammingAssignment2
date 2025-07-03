# The functions below create a matrix object that can be inverted and cached, 
# which then can be retrieved if the inverse has not been calculated before.

## A list of functions to get/set a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) { # set the matrix
    x <<- y
    i <<- NULL
  }
  get <- function() {x} # get the matrix
  setinverse <- function(solve) {i <<- solve} # set the inverse
  getinverse <- function() {i} # get the inverse
  
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


# Calculates the inverse of the matrix given if the inverse hasn't already been
# calculated. Otherwise it retrieves from the cache and returns it.

cacheSolve <- function(x, ...) {
  i <- x$getinverse() # Attempt to get the inverse
  
  if (!is.null(i)) { # If inverse matrix exists, retrieve from cache
    message("Retrieving from cached data...")
    return(i)
  }
  
  data <- x$get() # Get the matrix
  i <- solve(data, ...) # Solve for the inverse
  x$setinverse(i) # Set the inverse in cache
  i
}
