## makeCacheMatrix & cacheSolve are able to cache potentially time-consuming computations
## in order to retrieve a cached value to save computting resources if the value has already been calculated

## makeCacheMatrix creates a special "matrix" object that caches its inverse


makeCacheMatrix <- function(x = matrix()) {
  ##assign var to hold cached value
    invX <- NULL
  
  ##function to set vector y to vector x
    set <- function(y) {
      x <<- y
      invX <<- NULL
    }
  
  ##function to get value of x
   get <- function() x
  
  ## set mean of invX
    setMean <- function(mean) invX <<- mean
 
  ## get mean of invX
    getMean <- function() invX
  
  ## return list vector of special matrix
    list(set = set, get = get, setMean = setMean, getMean = getMean)
}


## cacheSolve computes the inverse of the"matrix" returned by makeCacheMatrix, 
##if the inverse has already been calculated (and the matrix has not changed), then cachesolve retrieves the inverse from the cache


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    invX <- x$getMean()
  
  #if invX is not been calcuated previously return it's value
  
    if(!is.null(invX)) {
      message("retrieving cached value")
      return(invX)
    }
  
  data <- x$get()
  invX <- mean(data, ...)
  x$setMean(invX)
  return(invX)
}
