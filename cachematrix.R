##  The task is to write two functions that let us cache the inverse of a matrix and get 
## it from there instead of calculating every time we need it.

## The function makeCacheMatrix creates a list containing four functions that set and get 
## (i.e.cache) values of the matrix and its inverse in the cache.


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set_inv <- function(inv) i <<- inv
  get_inv <- function() i
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)

}


## The second function checks if the inverse of matrix 'x' was calculated before. In that 
## case gets it from cache. If not, calculates the inverse and sets its value in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$get_inv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$set_inv(i)
  i
  
}
