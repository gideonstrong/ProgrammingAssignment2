## write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y){
     x <<- y
     m <<- NULL
   }
   get <- function()x
   setmatrix <- function(solve) m <<- solve
   getmatrix <- function() m
   list(set=set,get=get,setmatrix=setmatrix,getmatrix=getmatrix)
   }

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache. 

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getmatrix()
  if(!is.null(m)){
    message("getting cache data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix,...)
  x$setmatrix(m)
  m
}
##Test
x=matrix(1:4,2,2)
m=makeCacheMatrix(x)
m$get()
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4
cacheSolve(m)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## Retrieving from the cache in the second run
cacheSolve(m)
## getting cache data
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5