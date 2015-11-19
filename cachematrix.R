
## This function makeCacheMatrix creates a special matrix()  by accepting a matrix as an Input and returns a list that cotaings 
# a function to
# 1. set the value of the Matrix
# 2. get the value of the Matrix
# 3. set the value of the Inverse of the Matrix
# 4. get the value of the Inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                       # set the inverse variable to NULL
        set <- function(y) {            # Initialises the variables
                x <<- y
                m <<- NULL
        }
        get <- function() x             # Function to return the Matrix contents
        SetInverse <- function(solve) m <<- solve     # function to set the value of inverse
        getInverse <- function() m                    # function to get the inverse of the matrix
        list(set = set, get = get,                    # return the list of functions.
             SetInverse = SetInverse,
             getInverse = getInverse)
}



## The following function calculates the inverse of the special "Matrix" created with the above function. 
# However, it first checks to see if the inverse  has already been calculated.
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse using solve() command on the data and sets the value of the inverse in the cache via the SetInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()        # Get the inverse of the matrix
        if(!is.null(m)) {          # if inverse is available, then return the value and save additional computation.
                message("getting cached data")
                return(m)
        }
        data <- x$get()           # No Inverse, so get the data using the x$get function
        m <- solve(data, ...)     # Compute the inverse using Solve R function by passing the data  
        x$SetInverse(m)           # Set the inverse by calling the SetInverse function
        m                         # Return the inverse 
        
}
