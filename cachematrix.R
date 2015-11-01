## There are two functions :
## makeCacheMatrix - this expects a matrix as input, and returns a list of functions 

makeCacheMatrix <- function(x = matrix()) {

## m is a variaable set within environment of this function and is not visible or usable outside the function      
    m <- NULL
    
## set --> which sets the value of input parameter x to y and sets the value of m variable, which has been declare in the environment outside the set function,  to NULL
    
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    
## get --> returns the matrix x which is passed to create the object of the main function
    get <- function() x

##  setinverse function expects the solve function to create the inverse of the matrix and set it to the variable m, which is declare outside the scope of the setinverse function
    
    setinverse <- function(solve) m <<- solve
    
##  getinverser function just returns the variable m    
    getinverse <- function() m
    
## Return the list of all the functions inside the main function
    list (set = set, get = get,
          setinverse = setinverse, 
          getinverse = getinverse)
  }


## Return a matrix that is the inverse of 'x' by using the above function - makeCacheMatrix
cacheSolve <- function(x, ...) {

## Declares the inversere by setting the variable m (in the current function environment scope) 
  m <- x$getinverse()
## If m is already set to a value in previous iteration of function call, retrieving the value from the cached variable  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
## If m is not set previously, getting the inverse of the matrix and setting it to m  
  data <- x$get()
  m <- solve(data, ...)
## Setting the inverser of the main function by calling setinverse function of the main object so that next time when the function is called, it will retrieve the 
## value from cache instead of calculating it again
  x$setinverse(m)
  m
}
