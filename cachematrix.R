## Put comments here that give an overall description of what your
## functions do
## There are two functions makeCacheMatrix and cacheSolve. 
##One is incomplete without other

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL    #variable for inverse
  set <- function(y){
    x <<- y                   ## double arrow for modifications
    inv <<- NULL
  }
  get <- function(){x}   #function to get matrix x
  setinv <- function(inverse) {inv <<- inverse}
  getinv <- function() {inv}
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)

}

## Write a short comment describing this function
##

cacheSolve <- function(x, ...) { ## Gets data from parent environment
  inv <- x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)   #inverse is calculated here
  x$setinv(inv)
  inv    ## Return a matrix that is the inverse of 'x'
        
}
