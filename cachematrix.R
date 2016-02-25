##  makeCacheMatrix returns a list of functions constructed 
##  by passing an invertible matrix
## https://github.com/ToniC-NYC/ProgrammingAssignment2.git




makeCacheMatrix <- function(x = matrix())  {
## Local instance holding a cached inverse matrix
  m <- NULL
  setMatrix <- function(y) {
## <<- Operator Functions allowing access to the function instance variables
    x <<- y
## Remove Cached value if new matrix applied
    m <<- NULL
  }
  getMatrix <- function() x
  setInverseMatrix <- function(InverseMatrix) m <<- InverseMatrix
  getInverseMatrix <- function() m
## Return a list of variables pointing to above function  
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## cacheSolve returns the cached inverse matrix by passing
## in an instance of the makeCacheMatrix function, otherwise
## generates a new inverse matrix 

cacheSolve <- function(x, ...) {
## Obtain cached value from function  
  m <- x$getInverseMatrix()
## If cached value exist, retrieve inverse matrix and exit function
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
## Else compute new inverse matrix
  data <- x$getMatrix()
  m <- solve(data,...)
  x$setInverseMatrix(m)
  m
}

