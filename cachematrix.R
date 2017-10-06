## two function 1. makeCacheMatrix() creates a special matrix, basically 
## set the matrix (inverse) and get matrix (inverse)
## 2. function is cachesolve() calculate inverse from matrix created by makeCacheMatrix(), 
## first it verifies if inverse has been calculated, otherwise, it calculates inverse. 



## objects X and minverse are initialized. these Objects store information. 
## getters functions access information,
## setters functions set information.
## finally, a list is created, where each element is a function (getters or setters.)

makeCacheMatrix <- function(x = matrix()) {
        minver <- NULL
        set <- function(y) {
                x <<- y            ##<< indicates that the assignment should be made to the parent environment,
                minver <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) minver <<- inverse
        getinverse <- function() minver
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'
## function try to acess to a inverse passed in the argument (list)
## the "if" checks if argument is NULL (is there any inverser?). conditional is TRUE check cache,
## if conditional is false use solve function to calculate inverse


cacheSolve <- function(x, ...) {
        minver <- x$getinverse()
        if(!is.null(minver)) {       
                message("getting cached data")
                return(minver)
        }
        data <- x$get()
        minver <- solve(data, ...)   
        x$setinverse(minver)
        minver       
}
