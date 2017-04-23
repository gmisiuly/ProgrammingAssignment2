## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix inputs an invertible matrix
## calculates the inverse of the matrix and caches it.

makeCacheMatrix <- function(x = matrix()){
  
  
  ## 1. set the matrix
  ## 2. get the matrix
  ## 3. set the inverse of the matrix
  ## 4. get the inverse of the matrix
  
  ## initially set to NuLL
  ## changes when the user sets the value
  
  inver <- NULL
  
  ## set function
  ## sets the matrix itself but not the inverse
  
  set <- function(y){
    x <<- y
    inver <<- NULL
    
  }
  
  ## get function
  ## gets the matrix itself but not the inverse
  
  get <- function() x
  
  ## manually set the inverse
  
  setinverse <- function(solve) inver <<- solve
  
  ## get the inverse
  
  getinver <- function() inver
  
  ## encapsulate into a list
  
  list (set = set, get = get,
        setinverse = setinverse,
        getinver = getinver)
}


## cacheSolve computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache. 

cacheSolve <- function(x,...) {
  
  
  ## get the current state of inverse and see
  ## if it has been computed yet
  inver <- x$getinver()
  
  ## if it has inverse, return cached inverse
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  
  ## if it does not have inverse yet:
  ##  get the matrix itself
  data <- x$get()
  
  ## find the inverse
  inver <- solve(data, ...)
  
  ## cache this inverse in the object 'inver'
  x$setinverse(inver)
  
  ## return the object 'inver',which is the new result
  inver
}