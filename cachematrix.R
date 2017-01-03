## This function complies with the following steps:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse matrix. but first checks 
## to see if the inverse matrix already been calculated. if so, it 
## gets the inverse matrix from the cache and skips the computation,
## if not, compute the inverse matrix and sets the value in the cache 
## via the setinverse function

cacheSolve <- function(x, ...) {
        
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
   data <- x$get()
   m <- solve(data, ...)
   x$setinverse(m)
   m
        ## Return a matrix that is the inverse of 'x'
}
##
   ##   X<-rbind(c(1,1,0),c(1,0,1),c(0,1,0))
   ##   x<-makeCacheMatrix(X)
   ##    cacheSolve(x)
   ##       [,1] [,2] [,3]
##     [1,]   1    0   -1
##     [2,]   0    0    1
##     [3,]  -1    1    1
      
      
  ##   cacheSolve(x)
## getting cached data
##       [,1] [,2] [,3]
## [1,]    1    0   -1
## [2,]    0    0    1
## [3,]   -1    1    1
