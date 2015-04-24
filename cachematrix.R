## These two functions are used to create a special matrix object and then store its inverse. 
##The inverse is cached so future requests don’t require recalculating the inverse. 
## Example,
##      a<-makeCacheMatrix(matrix(c(1,0,4,1,3,4,4,1,0),nrow=3,ncol = 3,byrow = TRUE))
##      cacheSolve(a)
##    [,1]        [,2]    [,3]
##    [1,]  0.08333333 -0.08333333  0.2500
##    [2,] -0.33333333  0.33333333  0.0000
##    [3,]  0.22916667  0.02083333 -0.0625
##      cacheSolve(a)
##    getting cached data
##    [,1]        [,2]    [,3]
##    [1,]  0.08333333 -0.08333333  0.2500
##    [2,] -0.33333333  0.33333333  0.0000
##    [3,]  0.22916667  0.02083333 -0.0625


## This function creates a special Matrix. It holds a list of four functions:
##  1. get which gets returns the stored matrix.
##	2. set, which sets the matrix (this is only used if you want to change the matrix).
##	3. setinversion, which sets a value to the variable inv.
##	4. getinversion, which returns the value of the variable inv.

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {
                  x <<- y
                  inv <<- NULL
                }
                get <- function() x
                setinversion <- function(solve) inv <<- solve
                getinversion <- function() inv
                list(set = set, get = get,
                     setinversion = setinversion,
                     getinversion = getinversion)
                
}


## This function take the special matrix created with the function makeCacheMatrix andcalculates its invers. 
##If the invers has already been calculated it will return the stored value in the special matrix,
## otherwise it will calculate the invers and store in the special matrix for future requests. 

cacheSolve <- function(x, ...) {
       
  inv <- x$getinversion()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinversion(inv)
  inv
}
