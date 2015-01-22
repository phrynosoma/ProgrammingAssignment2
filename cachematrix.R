## makeCacheMatrix and cacheSolve store and retrieve a matrix in a particular
##  environment and either compute its inverse, or retrieve the stored inverse 
##  if it has already been computed.
##  


## makeCacheMatrix produces a list of functions used to store and retrieve 
##   a matrix and its inverse in a particular environment. 
##   Input is a matrix x. Output used by cacheSolve function. 

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) s <<- inverse
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve checks to see if the inverse of matrix x has been computed 
##   and cached. If not, it computes the inverse and if so, returns the 
##   cached inverse of x. 
##   Input to cacheSolve is the list produced by the makeCacheMatrix 
##   function.

cacheSolve <- function(mCM.out, ...) {
        s <- mCM.out$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- mCM.out$get()
        s <- solve(data, ...)
        mCM.out$setinverse(s)
        s
}