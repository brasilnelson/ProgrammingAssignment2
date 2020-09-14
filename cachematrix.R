# Here we have two functions:
# `makeCacheMatrix`: a function that receives a matrix 'M' creates a special
# "matrix" object and caches the inverse of 'M'
#
# `cacheSolve`: a function function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix`. If the inverse has
# already been calculated (and the matrix has not changed), then this function
# retrieves the inverse from the cache.

## This function receives a matrix 'M' and set the inverse of 'M', 'Mi', to be 
## NULL after that, it creates three new functions:`getmat` that gets the matrix
## 'M', `setinv` that sets a matrix to be the inverse of 'M' and `getinv` that
## returns 'M^(-1)'.
## Finally, the value returned by this function is a list with three components
## (getmat, setinv, getinv), a "special matrix object"

makeCacheMatrix <- function(M = matrix()) {
  Mi <- NULL
  getmat <- function() M
  setinv <- function(solve) Mi <<- solve
  getinv <- function() Mi
  list(getmat = getmat,
       setinv = setinv,
       getinv = getinv)
}


## This function receives the "special matrix object" returned by 
## `makeCacheMatrix` i.e. return a matrix that is the inverse of 'M'
## First of all, the function gets the 'getinv' component of the list.
## If it is not a NULL entry, just return this value, i.e. 'M^(-1)'
## Now, if it is NULL, compute the inverse of 'M' and set 
## to the 'setinv' component.

cacheSolve <- function(M, ...) {
  Mi <- M$getinv()
  if(!is.null(Mi)) {
    message("getting cached data")
    return(Mi)
  }
  data <- M$getmat()
  Mi <- solve(data, ...)
  M$setinv(Mi)
  Mi
}