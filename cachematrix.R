### cachematrix.R contains two functions which work together to 
### find the inverse of a matrix and pulling the inverse from 
### the cache where possible in order for faster run times.

### assume the matrix x is always an invertible matrix
makeCacheMatrix <- function(x = matrix()) {

        ### initialize the Inverse function m and make it a matrix       
        m <- NULL

        ### initializes x and m in the parent environment
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ### assigns the x function to "get" and pulls it from the parent environment
        get <- function() x
        
        ### puts the inverse (solve) calculation in the parent environment for later retrieval
        setInv <- function(solve) m <<- solve
        
        ### assigns the m function to "getInv" and pulls it from the parent environment
        getInv <- function() m
        
        ### assigns names to the set, get, setInv, and getInv functions 
        ### for the parent environment so we can use $ later to call them
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
        
}


### Function will check to see if the matrix inverse exists in parent environment
### and if true, then will return the cached version.  Otherwise, it will perform
### matrix inverse computation via the solve function within this function
cacheSolve <- function(x = matrix(),...) {
       
        ### assigns the value from the getInv function defined above to m
        m <- x$getInv()
        
        ### checks to see if m has any values, therefore allowing to be pulled
        ### from the cached version.  If true, then displays message that the 
        ### returned value is from the cache and also displays the inverse matrix
        ### from the cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ### If cached version isn't found in previous step, the get function is called
        ### in order to proceed with the calculation steps here
        data <- x$get()
        
        ### assigns the inverse matrix value to m so it can be returned at the end of
        ### this function (this is NOT from cached version)
        m <- solve(data, ...)
        
        ### uses the above inverse matrix that was created (m) to set the inverse 
        ### value in the set function for future use
        x$setInv(m)
        
        ### returns the inverse matrix
        m
        
}
