## This script is splited into two parts: 
## makeCacheMatrix will create a list of function, as well as provide memory for matrix and inversematrix
##cacheSolve will check if inverse exist, then print if it exist, else create inverse.

## makeCacheMatrix create 4 functions. Set will set memory on a cacheMatrix, get will show the memorized matrix
## setinv will set inverse of matrix, getinv will return the inverse of matrix. inverse is memorized in m

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cachesolve takes in an object of "makeCacheMatrix". it then try to check if the inverse memory "m" in "makeCacheMatrix" is null or not
## if it is, generate an inverse and memorize in makeCacheMatrix object
## else just print out the inverse from memory.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

## This is the test suite, uncomment it to test
#mat <- diag(1,2500,2500) #inverting this should take 10 sec
#cachemat <- makeCacheMatrix()
#cachemat$set(mat) #set in the matrix
#cacheSolve(cachemat) #now solve it for the first time
#cacheSolve(cachemat) #second time - this should be significantly faster