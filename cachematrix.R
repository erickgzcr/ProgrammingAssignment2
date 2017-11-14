## The functions cache the inverse of a matrix.

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {      
# Declares function "makeCacheMatrix". The input of "makeCacheMatrix" is x, the matrix.
        m <- NULL                                      
        # Initializes m to NULL. 
        set <- function(y) {                           
                # Declares function "set", whose input is "y".
                x <<- y                                      
                # Assigns "y" value to x. 
                m <<- NULL                                   
                # Sets "m" to NULL.  
        }
        get <- function() x                            
        # Declares function "get"."get" returns "x", the matrix. 
        setinverse <- function(result) m <<- result    
        # Declares function "setinverse". It stores the result of the inverse calculation from cacheSolve to "m" in makeCacheMatrix. 
        getinverse <- function() m                     
        # Declares function "getinverse". It returns "m" in makeCacheMatrix. "m" is NULL the first time. 
        list(set = set, get = get,                     
             # Creates list with values.
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve<- function(x, ...) {                 
        m <- x$getinverse()                            
        # Calls "getinverse" function of makeCacheMatrix, which returns "m". "m" is NULL the first time. If not NULL, it returns the value stored during setinverse step.  
        if(!is.null(m)) {                              
                # Checks if "m" is different than NULL. If not, it is skipped. 
                message("Getting cached data")               
                # Prints "Getting cached data" if "m" is different than NULL. 
                return(m)                                    
                # Prints "m". 
        }
        data <- x$get()                                
        # Calls "get" function of makeCacheMatrix. "get" returns "x", which is the matrix. This is assigned to "data".                            
        m <- solve(data, ...)                          
        # Inverse is calculated, applying "solve" to "data". The result is assigned to "m" in cacheSolve. 
        x$setinverse(m)                                
        # Calls "setinverse" function of makeCacheMatrix. Passes "m" in CacheSolve thru "result" to "m" in makeCacheMatrix.
        m                                              
        # Prints "m"
}

