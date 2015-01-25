## This script is for creating a function 'makeCacheMatrix' to make a matrix x
## Create a funtion 'cacheSolve' to catch the running time of the inverse matric x

## Write a makeCacheMatrix function to make a matrix x

makeCacheMatrix <- function(x = matrix()) {
  v<-null
  set<-function(y) {
    v<<-null
  }
  get<-function() x
  setinvert<-function(solve) v <<- solve
  getinvert<- function() v
  list(set=set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


## Write a function cacheSolve to catch the running time of inverse matrix

cacheSolve <- function(x, ...) {
        v<-x$getinvert()
        if(!is.null(v)) {
          message("getting cached data")
          return(v)
        }
        data<-x$get()
        v<-solve(data, ...)
        x$setmean(v)
        v
}
