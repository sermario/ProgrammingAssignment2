#Functions to calculate the inverse of a matrix.

#This function will create a "matrix" object that can cache its inverse.
#It creates a list containing:
#1. set value of the matrix
#2. get value of the matrix
#3. set the value of the inverse matrix
#4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  invm<-NULL
  set<-function (y){
    x<<-y
    invm<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) invm<<-inverse
  getinverse<-function() invm
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


#This next function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
#It asumes the matrix is invertible.

cacheSolve <- function(x, ...) {
  invm<-x$getinverse()
  if(!is.null(invm)){
    message("getting cached data")
    return(invm)
  }
  data<-x$get()
  invm<-solve(data)
  x$setinverse(invm)
  invm
}
