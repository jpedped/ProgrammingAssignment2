## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix()
## This function accepts one argument representing a matrix assumed to be 
## inversable. 
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  ##set up the matrix and default solve value
  set <- function(y) {
    x <<- y
    s <<- NULL
  } 
  
  ##return the original matrix
  get <- function() x;
  
  ##set the solve value "s" equal to the solve value passed in. this "s" value
  ## is stored in a higher environment 
  setSolve <- function(solve) s <<- solve
  
  ##returns the value "s"
  getSolve <- function() s
  
  ##creates a list referencing the function names "set", "get", etc. to their
  ## functions
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


##This will take in the result of a makeCacheMatrix function call and display 
## the cached result if available or calculate the result and store it in the
## cache
cacheSolve <- function(x, ...) {
  ##get the solve value already calculated
  s <- x$getSolve()
  
  ##if the solve value is not NULL then inform the user the value is being
  ## returned from cache.
  if(!is.null(s)) {
    message("getting cached data")
  } else {
    ##if the solve value is NULL ... 
    
    ##calculate the solve value for the matrix 
    ## object contained within the object passed in
    data <- x$get()
    
    ##solve the inversible matrix
    s <- solve(data, ...) %*% data
    
    ##set the solve value of the calculated inversible matrix
    x$setSolve(s)
  }
  
  ##return the inversible matrix
  return(s)
}
