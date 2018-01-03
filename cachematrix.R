## Computing the inverse of a square matrix can be done with the solve function in R. 
##For example, if X is a square invertible matrix, then solve(X) returns its inverse.

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
{
        inv<-NULL
  set<-function(y){
    x<<-y
  inv<-NULL}
  get <- function() x

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- Solve(data, ...)
  x$setInverse(inv)
  inv
      
}
