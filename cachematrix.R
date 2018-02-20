## The following functions are structured to allow caching of the inverse of matrixes.

## makeCacheMatrix returns a list which acts as a matrix, but encapsulated in a list with a get, a set
## and a get/set for the cached inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
     set <- function(y){
          x <<- y
          inv <<- NULL
     }
     setInverse <- function(inverse) inv <<- inverse
     get <- function() x
     getInverse <- function() inv
     
     list(set = set, get = get,
               setInverse = setInverse,
               getInverse = getInverse)
}


## cacheSolve is the equivalent of solve, but works with a "matrix" list created with the makeCacheMatrix
## function.

cacheSolve <- function(a, ...) {
     inv <- a$getInverse()
     if(!is.null(inv)){
               message("Gettin cached inverse")
               return(inv)
     }
     matrix <- a$get()
     inv <- solve(matrix, ...)
     a$setInverse(inv)
     inv
}
