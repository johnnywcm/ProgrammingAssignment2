## The aim of writing these two functions is to cache the inverse the inverse of a matrix that
## may be used repreatedly in computations. As matrix inversion is computationally intensive 
## (especially when the dimensions of the matrix is large), caching its inverse with the pair
## of functions will cut down the time needed to recompute the inverse of the matrix.


## The function "makeCacheMartix" creates a special "matrix" object that stores the matrix and 
## caches its inverse. To be specific, this function returns a list containing a function to
## set the matrix, get the matrix, set the inverse of the matrix
## and get the inverse of the matrix. The function takes in a matrix which we assume it to be
## invertible. 


makeCacheMatrix <- function(x = matrix()) {
  ## matinv is an object that stores the inverse of the matrix x. First assign it as a NULL object.
  matinv <- NULL 
  
  set <- function(y) {
    ## When the function "set" is called, the matrix y is set as matrix object x under the 
    ## function "makeCacheMatrix" environment and not in the set function's environment. 
    x <<- y 
    ## When "set" function is called, the assignment NULL to matinv overwrites any previous
    ## assingments in the "makeCacheMatrix" environment
    matinv <<- NULL  
  }
  
  ## The function "get" extracts (i.e. returns) the stored matrix from the matrix object x
  get <- function() x
  
  ## This function's input is the matrix's inverse and it is assigned to the matrix object 
  ## matinv which overwrites any previous assignments of matinv in the "makeCacheMatrix"
  ## environment
  setinverse <- function(inverse) matinv <<- inverse
  
  ## 'getinverse' returns the matrix's inverse that is stored in the matrix object matinv
  getinverse <- function() matinv
  
  ## the function "makeCacheMatrix" returns a list with the 4 functions defined within 
  ## it.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the matrix's inverse has already been calculated and the matrix is unchanged, then 
## the cachesolve should retrieve the inverse from the cache. Otherwise, the function proceeds
## with calculating the inverse of the matrix and sets the inverse using the "setinverse"
## from the list created from makeCacheMatrix function. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matinv <- x$getinverse()
  
  ## A check is done here. If the matinv object is not NULL, then a message pops out to 
  ## indicate the inverse has been stored and returns the matrix's inverse from the matinv.
  ## The cacheSolve function will not proceed further after returning the inverse matrix.
  if(!is.null(matinv)) {
    message("getting cached data")
    return(matinv)
  }
  
  ## If the above check shows matinv is NULL, procced by first getting the matrix using the 
  ## "get" function, follow by calculating the inverse of 'x' using the solve function.
  ## Thereafter, store the calculated inverse using the setinverse function before returning
  ## the inverse matrix. 
  data <- x$get()
  matinv <- solve(data, ...)
  x$setinverse(matinv)
  matinv
}
