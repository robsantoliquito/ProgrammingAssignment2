###################################################################
## Course:                 R Programming                          #
## Programming Assignment: 2                                      #
## Start Date:             April 17, 2015                         #
## Complete Date:          April 24, 2015                         #
## Programmer:             Rob Santoliquito                       #
## R Version:              3.1.3                                  #
## Operating System:       Windows 7 Professional                 #
###################################################################

###################################################################
## OVERVIEW: This function will perform the following:            #
## 1. Create a square matrix                                      #
## 2. Cache, or store, the matrix for future reference            #
## 3. Verify if the matrix has already been inverted              #
## 4. If not, the function will invert the matrix and cache it    #
## 5. Otherwise the function will return the inverted matrix.     #
###################################################################

###################################################################
## FUNCTION 1: 'makeCacheMatrix'                                  #
## This function stores a list of four functions that will be     #
## used by a seperate function named 'cacheSolve'                 #
###################################################################

makeCacheMatrix <- function(x = matrix()) {
  m <-NULL 
  set <- function(y) {                      #sets the value of the matrix
    x <<- y                                 ###(A) overwrites the matrix x with the values of y
    m <<- NULL                              ###(B) overwrites the object m as NULL
  }
  get <- function() x                       #gets the value of the matrix
  setinverse <- function(solve) m <<- solve #sets the inverted matrix
  getinverse <- function() m                #gets the inverted matrix
  list  (
    set=set,                                #stores the function set     in the parent function makeCacheMatrix
    get=get,                                #stores the function get     in the parent function makeCacheMatrix
    setinverse=setinverse,                  #stores the function setmean in the parent function makeCacheMatrix
    getinverse=getinverse                   #stores the function getmean in the parent function makeCacheMatrix
  )
}

###################################################################
## FUNCTION 2: 'cacheSolve'                                       #
## This function returns the inverse of a matrix by:              #
## 1. Verifying if the matrix has already been inverted.          #
## 2. If yes, then the inverted matrix is returned.               #
## 3. If no, then the raw data is retrieved, inverted, and cached #
##    before returning the newly inverted matrix.                 #
###################################################################

cacheSolve <- function(x, ...) {
  m <- x$getinverse()                       #retrieves the inverse of the matrix
  if(!is.null(m)) {                         #checks to see if the matrix has already been inverted
    message("getting cached data")          #notifies user with a message
    return(m)                               #and returns inverted matrix
  }
  data <- x$get()                           #if the matrix is not inverted, then function retrieves raw data
  m <- solve(data, ...)                     #inverts the matrix
  x$setinverse(m)                           #sets the inverted matrix equal to m
  m                                         #prints the new value of m
}

###################################################################
## TEST:                                                          #
## This code verifies if functions 1 & 2 are working properly.    #
###################################################################

x <- makeCacheMatrix()
m <- matrix(c(-1, -2, 1, 1), 2, 2)
x <- makeCacheMatrix(m)
x$get()
inv <- cacheSolve(x)
inv
inv <- cacheSolve(x)
inv

