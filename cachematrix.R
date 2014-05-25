## Caching the Inverse of a Matrix..

## makeCacheMatrix returns a list of functions
## to set and get the value of a matrix and the inverse
## of that matrix

makeCacheMatrix <- function(x = matrix()) {
        ## Creates a list of functions that can cache 
        ## the inverse of a matrix.
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) m <<-inverse
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
        ## Returns a matrix
}

## cacheSolve computes the inverse of a matrix 
## returned by the makeCacheMatrix function. 
## If the inverse has already been calculated,
## it returns it from the cache.
cacheSolve <- function(x, ...) {
        
        m <- x$getInv()
        ## Returns a matrix that is the inverse of 'm'
        
        if (!is.null(m)) {                 
                message("getting cached data")
                return(m)
                ## Check if there is a matrix. 
                ## If not, calculate the inverse
        }
        
        m <- solve(x$get())
        x$setInv(m)
        ## Set the inverse of the matrix
        
        m
        ## Return a matrix that is the inverse of 'x'
}