## The following contains a pair of functions that cache the inverse of a matrix

## Function1 - makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}

## Function2 - cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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

## Code validation
aMatrix <- makeCacheMatrix(matrix(1:4,ncol=2,nrow=2))

aMatrix$get()
aMatrix$getinverse()

cacheSolve(aMatrix) 
aMatrix$getinverse() 
