## Programming R Assignment 2
## In this assigment we introduce the <<- operator used to assign a value to an object in an environment
## that is different from the current environment


## Caching the Inverse of a Matrix:
## When running time consuming computations it is good to cache the results to 
## easily look them up later versus running the computation again saving time.
## Matrix inverting is usually taxing especially when running in a loop
## Therefore, you can compute and cache the inverse of a matrix

## makeCacheMatrix() function creates a special "matrix" object that can cache its inverse. The function is 
## a list that will:
## 1. Create the matrix
## 2. Get the matrix
## 3. Create the inverse of the matrix
## 4. Get the inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    # assigning x and inv to objects in different environments
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This above list is used as the input to cacheSolve()
## cacheSolve() computes, caches and returns the inverse of
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x' which is the input to makeCacheMatrix()
  inv <- x$getInverse()
  # if the inverse has already been calculated, get it from the cache
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # otherwise calculate the inverse
  mat <- x$get()
  inv <- solve(mat, ...)
  # sets the value of the inverse in the cache with the setinv function. However, using solve with a single
  # parameter is a request to invert the matrix and if your matrix is singular, it cannot be inverted
  # and you will get an error
  x$setInverse(inv)
  inv
}

## there are various ways to contruct a matrix with functions matrix, rbind, cbind, etc.

## Sample Code
## Create a 2x2 matrix
##  > amatrix <- makeCacheMatrix(matrix(1:4,2,2))

## Get or show the matrix
##  > amatrix$get()

## Computes, caches and returns matrix inverse
##  > cacheSolve(amatrix)

## Returns the matrix inverse
##  > amatrix$getInverse()


