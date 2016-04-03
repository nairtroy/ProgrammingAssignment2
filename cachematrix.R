# A special "matrix" object is created by makeCacheMatrix function in order to cache its inverse.
# cacheSolve function calculates the inverse of the special "matrix" created by the first function.
# If the inverse has already been calculated, then the it should retrieve the inverse from the cache.

# makeCacheMatrix is the first function that creates a list containing a function to 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#The inverse of the matrix can be calculated using the following function. 
#It first checks to see if the inverse has been calculated and if so, the function skips the computation 
#and obtain the results. However, if the inverse has not been calculated, the function 
#calculates the inverse and sets the value in the cache. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
