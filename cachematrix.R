## This function take a matrix and create four functions: namely
## 1. set() to set a matrix, 2, get() to get the matrix, 4, to get inverset of
## given matrix and 4. to set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     inversed_M<-NULL
     set <- function(y) {
          x <<- y
          inversed_M <<- NULL
     }
     get <- function() x
     set_Inverse <- function(inverse) inversed_M <<- inverse
     get_Inverse <- function() inversed_M
     list(set = set, get = get,
          set_Inverse = set_Inverse,
          get_Inverse = get_Inverse)
}


## Return a matrix that is the inverse of matrix if the given matrix is 
## square invertible matrix. The function return the cached inverse matrix if it ## is in the cache and compute if not.

cacheSolve <- function(x, ...) {
     m <- x$get_Inverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$set_Inverse(m)
     m    
}
