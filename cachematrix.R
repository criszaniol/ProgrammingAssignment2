## This function allow to calculate inverse matrix and store the result in the cache memory,
## not being necessary to calculate inverse matrix if it had already been calculated

#This function creates a special "matrix" object that 
#can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  #assigns a matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  #return the matrix x
  get <- function() x
  #assigns a matrix inverse
  setinv <- function(inv) inverse <<- inv
  #return a inverse matrix
  getinv <- function() inverse
  #return list with function to manipulate matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


  #This function computes the inverse of the special
  #"matrix" returned by makeCacheMatrix above. If 
  #the inverse has already been calculated (and the
  #matrix has not changed), then the cachesolve 
  #should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  
  #receive inverse matrix
  inv <- x$getinv()
  #if inv=/NULL, recover inverse matrix 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #assigns the matrix x in data
  data <- x$get()
  #return the inverse 
  inv <- solve(data)
  #assigns to cache
  x$setinv(inv)
  inv
}
