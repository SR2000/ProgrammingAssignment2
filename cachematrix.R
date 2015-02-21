## Takes a matrix and makes it accessible through getters and setters

makeCacheMatrix <- function(x = matrix()) {
        ##initializes the matrix imverse to null
        i <- NULL
        ##sets the matrix
        set <- function(y) {
                ##sets the values and modifies them in the enclosing environment
                ##sets the matrix inverse to null since the cached value is no longer valid
                x <<- y
                i <<- NULL
        }
        ##gets the matrix
        get <- function()x
        ##inverts the matrix and caches it in the enclosing environment
        setinv <- function(solve) i <<- solve
        ##gets the matrix inverse
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Takes the matrix created by makeCacheMatrix and returns the inverse

cacheSolve <- function(x) {
        ##gets the inverse from the list in makeCacheMatrix
        i <- x$getinv()
        ##returns the cached non-null value of the matrix inverse
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ##gets the value of the underlying matrix
        data <- x$get()
        ##calculates the matrix inverse, sets it, caches it and returns it
        i <- solve(data)
        x$setinv(i)
        i
}
