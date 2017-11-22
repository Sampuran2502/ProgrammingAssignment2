##Function to create matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {inverse<-NULL
set<-function(y){
  x<<-y
  inverse<<-NULL
  
}
get<-function() x
setinv<-function() inverse<<- solve(x)
getinv<-function() inverse
list(set=set,get=get,setinv=setinv,getinv=getinv)


}

##Function to compute the inverse of the matrix

cacheSolve <- function(x, ...) {
  inverse<-x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
    
  }
  data<-x$get()
  inverse<-solve(data,...)
  x$setinv(inverse)
  inverse 
}
