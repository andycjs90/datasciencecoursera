## The purpose of these 2 functions are to cache potentially
## time-consuming computations of matrix inversion.

makeCacheMatrix <- function(x = matrix()) { ## A new matrix is assigned to the local variable x
        m <- NULL ## Assign NULL to variable m
        
        set <- function(y) {
            ## This function can be called to modify matrix
            x <<- y ## Variable x in the containing environment is assigned the value in variable 'y'
            m <<- NULL ## NULL is assigned to the variable m whenever the matrix in variable x is modified
        }
        
        get <- function() x ## Returns the matrix stored in variable x
        
        cacheMatrix <- function(matrix) m <<- matrix ## Cache matrix inverse in variable m found in the containing environment
        
        getInverse <- function() m ## Returns value in variable m
        
        list(set = set, get = get, 
             cacheMatrix = cacheMatrix,
             getInverse = getInverse) ## Create a list that stores the functions found in the containing environment 
        
        
} 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse() # Assign variable m from the "makeCacheMatrix" function to the local variable m
                        
        if(!is.null(m)) {
            message("getting cached data") 
            return(m) ## Return cached matrix stored in local variable m
        }
        data <- x$get() ##Store matrix from the "makeCacheMatrix" function
        m <- solve(data, ...) ## Compute matrix inverse and store the value in local variable m
        x$cacheMatrix(m) ## Cache matrix inverse in variable m found in "makeCacheMatrix" function
        m
    
}
