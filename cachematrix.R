makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  get <- function() m
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv

  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(m, ...) {
  inv <- m$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }

  data <- m$get()
  inv <- tryCatch( 
    solve(data, ...), 
    error = function(e) e)
  m$setinv(inv)
  return(inv)
}

## Example of usage:
#> A <- matrix(c(1,2,3,4), c(2,2))
#> cached <- makeCacheMatrix(A)
#> inv <- cacheSolve(cached)
#> inv
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> inv %*% A
#[,1] [,2]
#[1,]    1    0
#[2,]    0    1
#> inv <- cacheSolve(cached)
#getting cached inverse matrix
#> inv
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
