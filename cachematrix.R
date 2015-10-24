## makeCacheMatrix initializes a "Cache" environment in wich we store a matrix ('x')
## and its inverse ('inv') and provides functions (set/get/setinverse/getinverse) to use them.
## cacheSolve computes the inverse of 'x' matrix and updates "Cache" inverse variable ('inv')
## in 'x' "special matrix" environment, unless it is already setup/computed.

## makeCacheMatrix returns a list ("special matrix") of wich elements are functions
## and its "Cache" variables ('x','inv')

makeCacheMatrix <- function(x = matrix()) {
   
   ## Set to NULL "Cache" inverse variable
   inv <- NULL
   
   ## Initialize "Cache" variables (within makeCacheMatrix function environment):
   ## - set to y "Cache" matrix variable ('x'),
   ## - set to NULL "Cache" inverse variable ('inv')
   set <- function (y) {
      x <<- y
      inv <<- NULL
   }
   
   ## Get "Cache" 'x'
   get <- function() x
   
   ## Set "Cache" inverse variable ('inv'): 
   ## Use of <<- operator to set inverse variable ('inv')
   ## within makeCacheMatrix environement, not setinverse environment
   ## (else use of <- operator)
   setinverse <- function(inverse) inv <<- inverse
   
   ## Get "Cache" inverse variable ('inv')
   getinverse <- function() inv
   
   ## "Special matrix" as a result
   list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse of 'x'
## and updates (if need be) "Cache" inverse variable ('inv') in 
## 'x' "special matrix" environement

cacheSolve <- function(x, ...) {

   ## Set "local" inverse variable to x "special matrix" inverse "Cache" variable
   ## (within makeCacheMatrix environment related to x)
   inv <- x$getinverse()
   
   ## if "local" inverse variable is not NULL, it means that x inverse has already been
   ## computed and stored in x "special matrix" inverse "Cache" variable.
   ## Then returns already computed inverse matrix.
   if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
   }
   
   ## Else compute 'x' matrix inverse and setup "Cache" inverse variable 
   ## within x "special matrix" environment (using x$setinverse(inv) function)
   data <- x$get()
   inv <- solve(data)
   x$setinverse(inv)
   inv
   
}
