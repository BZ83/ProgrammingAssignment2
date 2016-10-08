## Functions allow you to save("cache") the calculated inverse for a matrix, 
## saving computation time

## makeCacheMatrix creates the cache

makeCacheMatrix <- function(x = matrix(runif(100, 0, 100), 10, 10)) {
    i <- NULL
    set <- function(y){
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinverse = setinverse, getinv = getinv)
}


## cacheSolve recovers the cached value; if there is no value, it calculates 
## a value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv() 
    if(!is.null(i)) {
      message("getting cached inverse")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}


