## Cahing the inverse of a matrix

## makeCacheMatrix creates a special "matrix", which is a list of 4 functions:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        seti <- function(inverse) i <<- inverse
        geti <- function() i
        list(set = set,
             get = get,
             seti = seti,
             geti = geti)
}


## cacheSolve calculates the inverse of a special "matrix" created by 
## makeCacheMatrix.
## It first checks whether the inverse has already been calculated. 
## If so, it returns the inverse from cache. Otherwise, it caluclates 
## the inverse and set it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$geti()
        if (!is.null(i))
                message("getting cached data")
        else {
                i <- solve(x$get(), ...)
                x$seti(i)
        }
        i
}
