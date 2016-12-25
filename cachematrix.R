## Caching the Inverse of a Matrix 
## This will be divided in to 2 functions. 
## This function will create a special matrix which can cache its inverse.
## This is nothing but a list containing following 4 functions,
## 1.  set - sets the value of the matrix
## 2.  get - gets the value of the matrix
## 3.  setinverse - sets the inverse of the matrix
## 4.  getinverse - gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function (y) 
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list (set = set, get = get, 
        setinverse = setinverse,
        getinverse = getinverse)
}


## The following function inversion of the matrix (from makeCacheMatrix function).
## If the inverse has already been calculated and the matrix has not been altered,
## then the function will retrieve the inverse from cache, instead of recalculating.

cacheSolve <- function(x, ...) 
{
  m <- x$getinverse()
  if (!is.null(m))
  {
    message ("getting cached matrix.")
    return (m)
  }

  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

