makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## this function has 4 items set, get, setinverse and getinverse.
  ##set gives the value of the matrix, get returns the value of the matrix, 
  ##setinverse gives the value of the matrix and getinverse gives the value of
  ##the inverse of the matrix
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




## returns it
## OR 
## compute the inverse and store it in the cache
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

B <- matrix(c(1,2,3,4),2,2)
#solve(B) #We pretend that this cant't happen xD

B1 <- makeCacheMatrix(B)
cacheSolve(B1) #inverse returned after computation

