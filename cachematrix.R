## These 2 functions work together to cache the inverse of a square matrix
## It can be used to save time on computing the inverse of a square matrix
## if it has previously been calculated

## This function creates 4 sub-functions to set, get, set inverse or get 
## inverse of a matrix "x".  It returns a list of these 4 functions

makeCacheMatrix <- function(x = matrix()) {
       inverse_x <- NULL #set variable to cache inverse of x
  
       set <- function(y) {
           x <<- y    
           inverse_x <<- NULL    
       }
  
       get <- function() x    
       
       setinverse<- function(inverse) inverse_x <<-inverse    
       
       getinverse <- function() inverse_x    
       
       list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)    #return a list of functions

}


## This function takes the output of makeCacheMatrix and 
## returns the inverse of a matrix that was fed into makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve retrieves the inverse from makeCacheMatrix (inverse_x)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_x <- x$getinverse()  #get inverse from x
  
  #check if inverse of x exists, if yes return it, if not calculate, set and return it
  if (!is.null(inverse_x)) {
    message("getting cached inverse matrix")
    return(inverse_x)
  } else {
    inverse_x <- solve(x$get())
    x$setinverse(inverse_x)
    return(inverse_x)
  }
}
