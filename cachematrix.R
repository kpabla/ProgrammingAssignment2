# Assignment for chaching matrix

# makeCacheMatrix : creates a vector (list) of functions
## set the value the matrix
## get the value the matrix
## set the inverse matrix using solve()
## get the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    get <- function() x

    setmatrix <- function(solve) m <<- solve

    getmatrix <- function() m

    list(set=set, get=get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)
}

# chacheSolve computes inverse of the special matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if (!is.null(m)) {
        message ("getting cached inverse of matrix")
        return (m)
    }
    matrix <- x$get()

    ## it will be good to check if det(matrix) is zero or not

    m <- solve (matrix, ...)
    x$setmatrix (m)
    m
}
