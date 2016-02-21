## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly (there are also alternatives to 
## matrix inversion that we will not discuss here). Your assignment is to write a pair of 
## functions that cache the inverse of a matrix.

## USAGE: cacheSolve(makeCacheMatrix(x))

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  
  s
}


## TESTS

testCacheSolve = function(mat){
  cacheMat <- makeCacheMatrix(mat)
  
  start.time = Sys.time()
  
  cacheSolve(cacheMat)
  print(paste("First solve (uncached)", Sys.time() - start.time, sep = ":"))
  
  start.time = Sys.time()
  cacheSolve(cacheMat)
  print(paste("Second solve (cached)", Sys.time() - start.time, sep = ":"))
}

runTestCacheSolve <- function() {
  set.seed(1)
  testCacheSolve(matrix(rnorm(1000000), nrow=1000, ncol=1000))  
}
