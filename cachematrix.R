# The functions work together to recieve the value of a matrix, calculate the
# inverse of said matrix and save it in order to be able to retrieve the 
# the value of the inverse without having to perform the calculation every time

# -----------------------------------------------------------------------------

# The makeCacheMatrix function creates a special "vector", which is
# a list containing functions that

#   1.  set the value of the matrix
#   2.  get the value of the matrix
#   3.  set the value of the Inverse matrix
#   4.  get the value of the Inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL   
    set <- function(y) {        #writes input matrix
        x <<- y
        I <<- NULL
    }
    get <- function() x         #reads input matrix
    setInverse <- function(Inverse) I <<-Inverse    #writes inverse matrix
    getInverse <- function() I      #reads inverse matrix
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# The cacheSolve function calculates the Inverse matrix and uses the special "vector"
# created with the above function. It first checks to see if the
# inverse has already been calculated. If so, it gets the inverse ("getInverse") from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the matrix and sets the value of the inverse in the cache via the `setInverse`
# function.

cacheSolve <- function(x, ...) {
    I <- x$getInverse()  
   
    if(!is.null(I)) {   #check to see if the inverse has already been calculated and cached
        message("getting cached data") #if it has, show message and return I
        return(I)
    }
    #if I is NULL, it calculates the inverse matrix using the solve() function
    matrix <- x$get() 
    I <- solve(matrix, ...)
    x$setInverse(I)     #sets value of the inverse matrix
    I    # Return a matrix that is the inverse of 'x'
}
