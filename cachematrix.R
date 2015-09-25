####################################################################
##
##  Simple Usage
##      # 1. Create a simple _SQUARE_  matrix
##      m <- matrix(c( 3, 1, 5, 7, 9, 13, 4, 15, 11),  nrow=3, ncol=3)     
##
##      # 2. Create an instance of the matrixCache object
##      mc <- makeCacheMatrix(m)
##        
##      # 3. Create an instance of the matrixCache object
##      cacheSolve(mc)
##        
##      #4. Run it again and check to see that the message about using the cache appears
##      cacheSolve(mc)
##
####################################################################


####################################################################
####################################################################
## Makes a object call makeCacheMatrix, which takes argument x as the base type of the matrix
####################################################################
makeCacheMatrix <- function(x = numeric()) {
  
  #Internal cache
  cacheMatrix <- NULL
  
  #clears out the current matrix, and resets the value stored in it.
  set <- function(y) {
    x <<- y
    CacheMatrix <<- NULL
  }
  
  #Gets the information that this was started with
  get <- function() x
  
  #Takes the input and uses the solve command to calculate the inverse.  This is then cached 
  setinverse <- function(inMatrix) cacheMatrix <<- solve(inMatrix)
  
  #Gets the previously cache matrix
  getinverse <- function() cacheMatrix
  
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


####################################################################
####################################################################
# Use the function passed in that has to have the functions get, set, 
# getinverse, and setinvers to operate on a matrix to get its invers
# or the cache of the inverse 
####################################################################
cacheSolve <- function(x, ...) {
  
  #Check to see if this already is populated
  m <- x$getinverse()
  if(!is.null(m)) {
    message("-- Using Cached Data --")
    return(m)  ## Return the value that was stored in the cache
  }
  
  #if it is not set, get the data passed in originally, and then setup the inverse
  data <- x$get()
  x$setinverse(data)  #it will automatiicaly, in the other function, perform the solve when the matrix is passed in
  
  #return the value that was just computed
  x$getinverse()
}
