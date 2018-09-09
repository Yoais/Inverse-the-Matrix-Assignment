## Inverse the matrix

## Write a short comment describing this function

makeCacheMatrix <-function(x = matrix()) {
  m<-NULL
  setMatrix<-function(y){
    x<<-y
    m<<-NULL
  }
  getMatrix <- function() x
  setinverse<-function(inv)m<<-inv
  getinverse<-function()m
  list(setMatrix=setMatrix,getMatrix=getMatrix,setinverse=setinverse,getinverse=getinverse)
}
#Calculates the inverse matrix using solve function
cachesolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$getMatrix()
  m <- solve(matrix, ...)
  x$setinverse(m)
  m
  
}
#Testing


