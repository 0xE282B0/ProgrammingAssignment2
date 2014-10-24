## Cachematrix is a set of functions which can be used to save 
## recources by storeing computation results.

## The makeCacheMatrix function wrapes the matrix and provides a
## getter and a setter function.
makeCacheMatrix <- function(x = matrix()) {
  ## start with an empty inverse var
  inverse <- NULL
  
  ## The set function sets the matrix and resets the value of inverse to NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## The get function returns the matrix
  get <- function() x
  
  ## The setinverse function stores the inverse matrix
  setinverse <- function(i) inverse <<- i
  
  ## The getinverse function returns the inverse matrix  
  getinverse <- function() inverse
  
  ## Make the functions available
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function uses the cached matrix to solve the matrix if not already solved
cacheSolve <- function(x, ...) {
  ## get the inverse matrix if it is alredy computed
  inverse <- x$getinverse()
  ## if the inverse is available notify the user and return the inverse matrix
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## otherwhise get the matrix from the cached matrix...
  data <- x$get()
  ## ...compute the inverse...
  inverse <- solve(data)
  ## ... and store the inverse matrix in the cached matrix
  x$setinverse(inverse)
  ## finaly return the inverse matrix
  inverse
}

## > c=rbind(c(1, -1/4), c(-1/4, 1))
## > m <- makeCacheMatrix(c)
## > cacheSolve(m)
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > cacheSolve(m) %*% c
## getting cached data
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1
