## function: makeCacheMAtrix
## argument: matirx - default empty matrix
## descripton: it stores a cahced copy of an inverse of matrix and provide accessor methods to get, set the matrix and its inverse

## Comment: The function declares a local cahce variable to store the inverted matrix. It declares four get and set functions. These functions are finally put in a list and returned. 

makeCacheMatrix <- function(x = matrix()) {
  
  ## cache variable
  cache <- NULL
  
  ## set: set the new matrix to x whcih is outside of the function's current scope
  set <- function(y){
    x <<- y
    cahce <<- NULL
  }
  
  ## get: returns the current value of x
  get <- function() {x}
  
  ## setInv: sets the inverse matrix to cache variable
  setInv <- function(inv){
    cache <<- inv
  }
  
  ## getInv: return the value of cache
  getInv <- function() cache
  
  ## create a list with the above functions
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## function: cacheSolve
## argument: x - a cacheable matrix from makeCacheMAtrix
## descripton: The function returns the cached version of inverted matrix or create new one if it does not already exists in cache

## Comment: The function first gets the inverted matrix from the cachedmatrix. If cachedmatrix is not the then value is returned from cahce. Otherwise an inverted matrix is calculated,stored in the cachedmatrix and returned. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  
  ## check if the inverse is null
  if (!is.null(inv)){
    message("from cahce")
    return(inv)
  }
  
  ## calculate the inverse and store in 'x'
  mat <- x$get()
  inv <- solve(mat)
  x$setInv(inv)
  
  ## return inverse
  inv
}

## Test Result

## Create a cached matrix
## mn <- makeCacheMatrix(matrix(c(3,-5,6,-7,5,2,-4,5,5), nrow=3, ncol=3))

## First time
## > cacheSolve(mn)
## output 
## [,1]       [,2]        [,3]
## [1,] -0.08333333 -0.1500000  0.08333333
## [2,] -0.30555556 -0.2166667 -0.02777778
## [3,]  0.22222222  0.2666667  0.11111111

## Second Time
## > cacheSolve(mn)

## output
## from cahce
## [,1]       [,2]        [,3]
## [1,] -0.08333333 -0.1500000  0.08333333
## [2,] -0.30555556 -0.2166667 -0.02777778
## [3,]  0.22222222  0.2666667  0.11111111
