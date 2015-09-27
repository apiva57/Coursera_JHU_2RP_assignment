## Functions makeCacheMatrix and cacheSolve will cache result of inverting for 
## the matrix.
## If function is called for the first time, than inversed matrix will be 
## calculated and stored in the cache. If inversion of the same matrix is needed 
## again than cached matrix will be return instead of doing all calculations
## again.

## makeCacheMatrix creates a special "matrix", which is really a list containing 
## a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) inverse <<- solve
      getInverse <- function() inverse
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

## The cacheSolve function calculates the inverse of the special "matrix" list
## created by the function makeCacheMatrix. However, it first checks to see 
## if the inverse matrix has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates the inverse 
## of the data and sets the value of the inverse in the cache via the setInverse 
## function.

cacheSolve <- function(x, ...) {
      inverse <- x$getInverse()
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setInverse(inverse)
      inverse
}

## Let's create matrix for testing
matrix <- rbind (c(1,0,4), c(1,3,4), c(4,1,0))
## Let's verify that inverse matrix exist for this one
solve(matrix)

## Let's call makeCacheMatrix for our matrix and verify that if we will call it
## for a second time than we will be getting cached data
cacheMatrix <- makeCacheMatrix(matrix)
cacheSolve(cacheMatrix)
cacheSolve(cacheMatrix)


#below is an original example for the vector and testing sample
makeVector <- function(x = numeric()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmean <- function(mean) m <<- mean
      getmean <- function() m
      list(set = set, get = get,
           setmean = setmean,
           getmean = getmean)
}

cachemean <- function(x, ...) {
      m <- x$getmean()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- mean(data, ...)
      x$setmean(m)
      m
}
x <- makeVector(c(1,3,5,7))
cachemean(x)
cachemean(x)