## The following two functions are used to create a special object that sotres a matrix and 
## cache's its inverse.
## Example input and output:
##           > m <- matrix(1:4, nrow = 2, ncol = 2)
##           > cacheSolve(makeCacheMatrix(m))

## The first function, makeCacheMatrix, creates a special "matrix" object, which is a list containing a function
## to set the matrix, get the matrix, set the inverse, and get the inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y){
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i[ , ]
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" created with the above function. 
## If the inverse has already been calculated (and the matrix has not changed), then this 
## function shoud retrieve the inverse from the cache and skip the computation. Otherwise, it 
## calculates the inverse and sets the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
