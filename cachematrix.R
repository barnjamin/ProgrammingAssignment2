## These functions are designed to prevent the cost of recalculation of the 
## inverse of an invertable matrix.  They should be used when one may need to 
## get the inverse of a matrix multiple times.



## Takes a matrix as its only argument and returns a list of functions 
## to get/set the matrix and get/set the inverse 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i

    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Takes a list returned by makeCacheMatrix and variable length arguments 
## and returns the inverse of x and caches the inverse of the matrix if it isnt
## already cached

cacheSolve <- function(x, ...) {
    i <- x$getInverse()

    if(!is.null(i)){
        message("Using cached data")
        return(i)
    }

    mat <- x$get()
    i <- solve(mat)
    x$setInverse(i)
    i
}

