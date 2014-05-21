## Put comments here that give an overall description of what your
## functions do

## Create special matrix with the functions set, get, setSolve and GetSolve

makeCacheMatrix <- function(x = matrix()) {
  #set value null to varibale S
  s <- NULL
  
  #function that set a value in variable x, and set null to variable s
  set <- function(y)
  {
    x <<- y
    s <<- NULL
  }
  
  #function that returns the value of x
  get <- function() x
  
  #function that updates cache value
  setSolve <- function(solve) s <<- solve
  
  #function that returns the cache value
  getSolve <- function() s
  
  #list of functions
  list( set = set, get = get, setmean = setmean, getmean = getmean)
}


## Function that solves the special matrix previously created
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  #set de cache value into s variable
  s <- x$getSolve()
  
  #evaluate cache value 
  if(!is.null(s))
  {
    #if is not null print a message and return the cache value
    message("getting cached data")
    return (s)
  }
  
  #if cache is null solves the matrix
  #set into variable data the value of matrix
  data <- x$get()
  
  #solve the matrix with the R function solve(a,b, ....) and assign into variable s
  s <- solve(x)
  
  #set result into special matrix
  x$setSolve(s)
  
  #return the result
  s
}
