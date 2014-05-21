## Put comments here that give an overall description of what your
## functions do

## Create special matrix with the functions set, get, setSolve and GetSolve

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y)
  {
    x <<- y
    s <<- NULL
  }
  
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  list( set = set, get = get, setmean = setmean, getmean = getmean)
}


## Function that solves the special matrix previously created

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getSolve()
  
  if(!is.null(s))
  {
    message("getting cached data")
    return (s)
  }
  data <- x$get()
  s <- solve(x)
  x$setSolve(s)
  s
}
