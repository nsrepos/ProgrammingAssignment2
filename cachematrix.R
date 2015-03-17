## Computing the inverse of a matrix is a time consuming computation
## especially for large size matrices.  If the matrix and it's inverse is not ## changing we can save computations by caching them in memory so that in the
## event we 
## need them we can return the cache value rather than re-compute them


## The following function `makeCacheMatrix` creates a special "matrix",
## which is a list containing a function to: 

## set /get value of the matrix

## set/get value of cached inverse of the matrix




makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The following function ‘cacheSolve’ calculates the inverse of the special 
## "matrix" created with ‘makeCacheMatrix’.
## If the inverse has already been calculated it returns the inverse from 
## the cache thus the computation is avoided.
## Otherwise, it calculates the inverse and sets the value of the inverse in
## the cache.

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    inv
    message("getting cached inverse")
    return(inv)
  }
  
  data <- x$get()
  
  inv <-solve(data, ...)
  
  x$setinverse(inv)
  
  inv
}
