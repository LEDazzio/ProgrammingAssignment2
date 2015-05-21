## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {           ##function defintion takes a matrix as an argument
  inv <- NULL                                         ##setting the inverse equal to null
  set <- function(y) {                                ##this function sets the values of x and inv outside the scope of the...                           
    x <<- y                                           ##...local environment
    inv <<- NULL
  }
  get <- function() x                                 ##this function gets the matrix argument 
  setinverse <- function(inverse) inv <<- inverse     ##this function gets the value of the inverse from outside the local environment and sets the value
  getinverse <- function() inv                        ##this function returns the value of the inverse
  list(set = set, get = get,                          ##this function defines a list of functions in the makeCacheMatrix object
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {    ##function definition takes an argument x
  inv <- x$getinverse()             ##sets the variable inv equal to the output of the getinverse function
  if(!is.null(inv)) {               ##if the inverse has already been calculated for a particular matrix...
    message("getting cached data")  ##...a message is displayed in the console...
    return(inv)                     ##...and the chached inverse is displayed
  }
  data <- x$get()                   ##if the inverse hasn't been computed, data is set to the value of the current matrix argument
  inv <- solve(data)                ##the inverse of the matrix is computed
  x$setinverse(inv)                 ##the value of the inverse is now set in the list object
  inv                               ##the computed inverse is displayed
}
