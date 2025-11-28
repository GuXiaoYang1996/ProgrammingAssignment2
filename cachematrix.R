## Caching the inverse of a matrix

## Creating a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inversematrix <- NULL
  set <- function(y){
    x <<- y
    inversematrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inversematrix <<- inverse
  getinverse <- function() inversematrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Computing the inverse of the special "matrix" returned by makeCacheMatrix above 
## If the inverse has already been calculated, retrieving the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversematrix <- x$getinverse()
  if(!is.null(inversematrix)){
    message("getting cached data")
    return(inversematrix)
  }
  data <- x$get()
  inversematrix <- solve(data,...)
  x$setinverse(inversematrix)
  inversematrix
}
