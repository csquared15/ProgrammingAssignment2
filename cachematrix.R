## These functions quickly cache the inverse of a matrix
## to avoid computing repeatedly

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        c <- NULL
        set <- function(y){
                x <<- y
                c <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) c <<- inverse
        getinverse <- function() c
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        c <- x$getinverse()
        if(!is.null(c))  {
                message("getting cached data")
                return(c)
        }
        data <- x$get()
        c <- solve(data, ...)
        x$setinverse(c)
        c
}
