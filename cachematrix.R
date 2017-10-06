


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
