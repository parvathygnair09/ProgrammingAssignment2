## The below function can find the inverse of a given matrix and each time the
## function is called it checks if the inverse is already calculated and if so,
## takes it from the cache instead of recomputing.

## The below function creates a list that sets the value of the input matrix,
## get its value, set the inverse and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## This function checks to see if the inverse of the input matrix is already in memory and 
## and gives the output.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
