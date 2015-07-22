## computation Inverse of a Matrix and cach the result.
## so when next time, the same matrix comes, return the caching 
## result instanding of computation repeatedly.

## make a structure to cache matrix
## args: x = matrix(), a matrix, or nothing when call this function.
## retrun: a list consist of the methods get/set original matrix and
## the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  ivs <- NULL
  ## if matrix is changed , then re assign it,
  ## and set the cache iverse NULL
  set <- function(y) {
    if (nrow(x) != nrow(y)
        || ncol(x) != ncol(y)
        || any(x!=y)) {
      x <<- y
      ivs <<- NULL
    }
  }
  get <- function() {
   x 
  }
  setivs <- function(ivs) {
    ivs <<- ivs
  }
  getivs <- function() {
    ivs
  }
  list (set = set, get = get,
        setivs = setivs,
        getivs = getivs)
}

## computation the inverse of a matrix, and cache it
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  x_matrix <- x$get()
  if (nrow(x_matrix) == ncol(x_matrix)
      && det(x_matrix) != 0) {
    # if x_matrix is a square invertible matrix, 
    # then returns its inverse
    x_ivs <- x$getivs()
    if (!is.null(x_ivs)) {
      message("get caching inverse matrix.")
      return (x_ivs)
    }
    message("computation the inverse matrix.")
    ivs <- solve(x_matrix)
    x$setivs(ivs)
    return (ivs)
  } else {
    message("invalid matrix, can not compute inverse.")
  }
}