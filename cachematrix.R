## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix and caches the inverse 

makeCacheMatrix <- function(x = matrix()) {
        i = NULL
        set = function(y) {
                x <<- y
                i <<- NULL
        }
        get = function() x
        setinv = function(inverse) i <<- inverse 
        getinv = function() i
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function checks if a matrix has already been inverted and returns 
## that value if so

cacheSolve <- function(x, ...) {
        i = x$getinv()
        if (!is.null(i)){
                message("getting inversed matrix")
                return(i)
        }
        data = x$get()
        i = solve(data, ...)
        x$setinv(i)
        return(i)
}

