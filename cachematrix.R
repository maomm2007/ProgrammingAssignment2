
## The 'makeCacheMatrix' function creates a special “matrix” object 
## that can cache its inverse.
## library(MASS) is used to calculate inverse for non squared as well as square matrices
library(MASS)
makeCacheMatrix <- function(x = matrix ()) {
  ## initializing inverse value as NULL
  inv <- NULL
  set <- function(y){
                    x <<- y
                    inv <<- NULL
                    }
  ## function to obtain matrix x
  get <- function()x
  setinv <- function(inverse) inv<<-inverse
  ## function to get matrix x
  getinv <- function() { 
                        inver <-ginv(x)
                        inver%*%x
                        }
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function computes the inverse of the “matrix” created by
## above makeCacheMatrix function.
## below gets cache data
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  ## validate if inverse is null
  if(!is.null(inv)){
    message("getting cached data")
  ## return inverse value
    return(inv)
  }
  data <-x$get()
  ## calculate inverse value
  inv <-solve(data, ...)
  x$setinv(inv)
  ## return a matrix that is the inverse of 'x'
  inv
}
