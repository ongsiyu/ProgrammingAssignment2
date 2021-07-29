## Creating two functions: makeCacheMatrix and cacheSolve

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){  ##set value of matrix using function y
    x <<- y   ## <<- used in parent level of managing function
    inv <<- NULL
  }
  get <- function(){x}  ##get value of matrix
  setInverse <- function(inverse){inv <<- inverse}  ##set value of inverse
  getInverse <- function(){inv}  ##get value of inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##cacheSolve will compute inverse if the matrix created by makeCacheMatrix function

cacheSolve <- function(x,...){  
  inv <- x$getInverse()  ##returns matrix inverse of x, assigning it to inv
  if(!is.null(inv)){  ##to check if inv is already calculated
    message("getting cached data")  
    return(inv)  ##inv retrieved from cache if available
  }
  mat <- x$get()  
  inv <- solve(mat,...)  ##compute inverse of matrix
  x$setInverse(inv)  ##set value of inverse in cache
  inv
}
