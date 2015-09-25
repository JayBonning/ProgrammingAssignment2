
## Makes a object call makeCacheMatrix, which takes argument x as the base type of the matrix
makeCacheMatrix <- function(x = numeric()) {
  
  #Internal cache
  cacheMatrix <- NULL
  
  #Set Function clears out the current value and sets the new matric type passed in
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
