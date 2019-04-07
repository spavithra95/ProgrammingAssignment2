#Creates a special matrix along with four functionalities
makeCacheMatrix <- function( x = matrix() ){
  i <- NULL
  
  #1. set the value of the matrix
  #reinitiate inverse to be Null
  set <- function( y ){
    x <<- y
    i <<- NULL
  }
  #2. get the stored data
  get <- function() x
  #3. get the stored inverse
  getinverse <- function() i
  #4. set the value of inverse
  setinverse <- function( inver ) i <<- inver
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#Solves the matrix and returns the inverse
#If the special matrix already has inverse returns the stored value
#Else calculates the inverse and stores in the special matrix
cacheSolve <- function( x ){
  inver <- x$getinverse()
  if(!is.null(inver)){
    print( "returning the cached data" )
    return(inver)
  }
  mat <- x$get()
  inver <- solve(x)
  x$setinverse(inver)
  inver
}