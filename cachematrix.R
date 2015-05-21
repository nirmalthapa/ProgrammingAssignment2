## The makeCacheMatrix returns special "matrix" which has following functions
## get the matrix
## set the matrix
## get the inverse of matrix
## set the inverse of matrix
## Parameter: regular matrix
## Returns: Special Matrix
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL 
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setInv <- function(invM) inv <<- invM
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve get the inverse of the matrix
## It first checks if it already exists in the cache 
## if is exist
##    returns the inverse
## else
##    calculates the inverse
##    saves into the cache
##    returns the inverse
## Parameter: the special Matrix creted by makeCacheMatrix
## Returns: Inverse of matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv 
}