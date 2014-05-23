## Utiliy methods to cache the inverse of a matrix

## Write a short comment describing this function
# Returns a list of 4 functions
#  get()   returns the value of the matrix
#  set()   sets the value of the matrix
#  getinverse()  returns the value of the inverse of the matrix
#  setinverse()  sets the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# returns a matrix that is the inverse of 'x'
# uses cached value if it exists,
# otherwise calculates value and
# stores it in the cache
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
}

