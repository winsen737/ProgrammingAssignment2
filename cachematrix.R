

## The role of the following functions is to cache the 
## inverse of a matrix rather than compute it repeatedly. 

## The first function, makeCacheMatrix creates a list containing:

## a function to set the value of the matrix
## a function to get the value of the matrix
## a function to set the inverse of the matrix
## a function to get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- matrix()
        set <- function(y){
                x <<- y
                inverse <<- matrix()
        }
        get <- function() x 
        
        setinverse <- function(y) inverse <<- y 
        
        getinverse <- function() inverse 
        
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function checks first to see if the inverse has already been
## calculated.If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the data and sets
## the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        
        inverse <- x$getinverse()
        if (!identical(inverse, matrix())) {
                message("getting cached matrix inverse")
                return(x$getinverse())
        }  
        
        matrix <- x$get()
        inverse <- solve(matrix)
        x$setinverse(inverse)
        inverse 
        
}
