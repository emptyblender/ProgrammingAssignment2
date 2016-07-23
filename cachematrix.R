
##These functions first reset the objects x and mI in the function environment.  
##Next, the set function establishes the object x (a matrix), and then
##another object, mI (the inverse), is established as an empty matrix, this time
##establishing/resetting them in the parent environment.  When applying 
##makeCacheMatrix,a matrix x and its mI are established with the set functions 
##then the values are retrieved with the get functions. This function is 
##designed to reduce processing speed by creating pointers to the parent
##environment so that set and get functions for x and mI are accessible 
##outside of the function using the '$' extractor because of the last section
##of code defining them as a list.  Finally, cacheSolve references the functions
##in makeCacheMatrix in order to calculate the inverse mI if it has not been
##calculated.


## makeCacheMatrix creates objects x(matrix) and mI(matrix inverse), uses 
## "set" function to define x and mI in the parent environment and uses 
## "get" function toget the values for x and mI.

makeCacheMatrix <- function(x = matrix()) {   ##defines x as a matrix
  mI <- NULL                                  ##creates mI as a null matrix
  set <- function(y) {                        ##sets x and mI in parent environment
    x <<- y
    mI <<- NULL
  }
  get <- function() x                        ##retrieves x and mI
  setinv <- function(inv) mI <<- inv
  getinv <- function() mI
  list(set = set, get = get,                 ##names the two set and two get functions
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve references the functions in makeCacheMatrix, determines whether mI
##has already been calculated, calculates mI using the solve() function.

cacheSolve <- function(x, ...) {
        
  mI <- x$getinv()                      ##retrieves the object mI if it is already calculated
  if(!is.null(mI)) {
    message("getting cached data")
    return(mI)
  }
  data <- x$get()                       ##defines the inputted values of matrix x as "data"
  mI <- solve(data, ...)                ##solves for the inverse of x
  x$setinv(mI)                          ##sets the inverse of x as "mI"
  mI                                    ##prints out mI
  }
