#This function generates a matrix of certain dimensions on classifying 
#the x object accordingly.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#This function evaluates the inverse for any given matrix which has been 
#generated via the MakeCacheMatrix Function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Matrix has been stored within the memory")
    return(i)
  }
  data <- x$get()
  i <- pseudoinverse(data, ...)
  x$setinverse(i)
  i
}


