## Roy de Groot
## Version 1.0


## Creates an alternative to a matrix to cache the inverse
## Setinverse to set the inverse, get inverse to get the inverse
makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
        set <- function(y) 
        {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Calculates the inverse of a matrix and stores it for the future
## if the inverse of the same matrix is already computed in the past
## it serves the stored inverse
cacheSolve <- function(x, ...) 
{
        m <- x$getinverse()
        
        if(!is.null(m)) 
        {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
