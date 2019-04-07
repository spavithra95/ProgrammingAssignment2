makeCacheMatrix <- function( x = matrix() ){
  i <- NULL
  set <- function( y ){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  getinverse <- function() i
  setinverse <- function( inver ) i <<- inver
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function( x ){
  inver <- x$getinverse()
  if(!is.na(inver)){
    print( "returning the cached data" )
    return(inver)
  }
  mat <- x$get()
  inver <- solve(x)
  x$setinverse(inver)
  inver
}