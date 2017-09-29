## Basic Overview: the following 2 functions are interdepent and cache a solved
## matrix in memory so that it can be accessed instead of solved repeatedly.

## The function makeCacheMatrix creates a special matrix vector which contains
## $ accessible functions which are returned at the end of the function at
## instantiation. At instantiation, it stores the matrix, and sets the variable
## holding the solved matrix to NULL.

## Assigning a variable to this function via
## variable <- makeCacheMatrix()
## makes "variable" a special object which is the only object useful to the
## cacheSolve function below it.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function (y) {
    x <<- y
    s <<- NULL
  }
  set(x)
  get <- function () x
  setsolve <- function (solved) s <<- solved
  getsolve <- function () s
  list(set = set,
       get = get,
       setsolve = setsolve,
       getsolve = getsolve
       )
}

## cacheSolve takes the special object created with makeCacheMatrix and returns
## the cached solved matrix after either putting it into the makeCacheMatrix
## object or after retrieving it from there.

## The solved matrix gets moved into the makeCacheMatrix's environment via
## calling of it's function returned in the list and <<- operator assigning
## the variable to the upper environment.

cacheSolve <- function(cachematrix, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## get s from the special object
  s <- cachematrix$getsolve()
  ## check if s is Null. if not null, return s from above
  if (!is.null(s)) {
    print("Getting cached matrix...")
    return(s)
  }
  print("Solving and caching matrix...")
  x <- cachematrix$get()
  solved <- solve(x)
  cachematrix$setsolve(solved)
  s <- cachematrix$getsolve()
  return(s)
}

## example usage:
## exampleMatrix <- matrix(c(4,2,7,6), nrow=2, ncol=2)
## cacheMatrixObject <- makeCacheMatrix(exampleMatrix)
## cacheSolve(cacheMatrixObject)
