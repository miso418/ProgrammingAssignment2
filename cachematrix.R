## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## First, you assign x as a certain invertible matrix.          eg. x=matrix(sample.int(81,9*9,replace=FALSE),9,9)
## You assign the result of makeCacheMatrix function as another name.           eg. mat=makeCaheMatrix(x)
## You can see the original vector with mat$get()


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## For the first run, there is no cache.
## After second run, cache is retrieved.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
