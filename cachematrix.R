## Put comments here that give an overall description of what your
## functions do

## Below function creates an object contains multiple functions wihch can help to cache it's inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        I<-NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) I <<- Inverse
        getInverse <- function() I
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Below function performs the calculation to get the inverse matrix provided by previous function "makeCacheMatrix"
## If the inverse matrix is already cached then this function will retrieve the result instead of performing calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getInverse()
        if(!is.null(I)) {
                message("getting Inverse data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setInverse(I)
        I
}