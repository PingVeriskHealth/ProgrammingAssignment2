## Put comments here that give an overall description of what your
## functions do
## This function is create a matrix function that caches its inverse, so that it would avoid repetitive calculation
## in the future if same calculation is needed

## Write a short comment describing this function

## makeCacheMatrix creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
                         }
      get <- function()x
      setInv <- function(inverse)inv <<- inverse
      getInv <- function()inv
      list(
      set = set, get = get, setInv = setInv, getInv = getInv
         )
}


## Write a short comment describing this function
## cacheSolve returns the inverse of the given matrix object 'x'. If the inverse
## has already been calculated and cached, cacheSolve just returns the cached inverse.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'+## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (is.null(inv)) {
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInv(inv)
      }
    inv
  }