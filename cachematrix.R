## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## it caches inverse of matrix x if its already calculated and it returns a list of functions
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function(x) x
  
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## Write a short comment describing this function
## calculates inverse of function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if (!is.null(inv)) {
    print("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
