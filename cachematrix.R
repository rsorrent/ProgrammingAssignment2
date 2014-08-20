## These functions allow to cache the result of the calculation 
## of the inverse of a matrix x in an environment different from the current environment
## So, instead of recomputing the inverse of x, we can recover the result from the cache, saving time.


## makeCacheMatrix create a list cointaining a function to 
## (1) set the value of the matrix,
## (2) get the value of the matrix,
## (3) set the value ot the inverse matrix
## (4) get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix())
        {
                m <- NULL
                set <- function(y)
                        {
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

## cachesolve checks if the inverse of the matrix created with macheCacheMatrix
## has already been calculated. In this case it gets the inverse from the cache. Otherwise 
## it calculate the inverse of that matrix and sets it in the cache

cachesolve <- function(x, ...) 
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
        

