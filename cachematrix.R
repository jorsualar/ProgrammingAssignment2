makeCacheMatrix<- function(x=matrix()) { 
  #x is any invertible matrix
  #these function will return a list with set,get,setinv,getinv functions
  #these functions with set/get the matrix and set/get the inverse
  #this list will be used by cacheSolve
  
  inv = NULL
  set = function(y){
    #<<- will help assign a value from a different environment
    x<<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv<<-inverse
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve<- function(x,...) {
  #x is output of makeCacheMatrix
  inv = x$getinv()
  if (!is.null(inv)) {
    #if inverse is null will get it from cache
    message("getting cached data")
    return(inv)
  }
  #if not will calculate the inverse
  mat.data <- x$get()
  inv <- solve(mat.data, ...)
  #will use setinv function to set inverse value in the cache
  x$setinv(inv)
  return(inv)
}
