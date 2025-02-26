# makeCacheMatrix Function
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL
    
    # Setter for the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset the cached inverse when the matrix changes
    }
    
    # Getter for the matrix
    get <- function() x
    
    # Setter for the inverse
    setInverse <- function(inverse) inv <<- inverse
    
    # Getter for the inverse
    getInverse <- function() inv
    
    # Return a list of functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# cacheSolve Function
cacheSolve <- function(x, ...) {
    # Check if the inverse is already cached
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)  # Return the cached inverse
    }
    
    # If not cached, compute the inverse
    data <- x$get()  # Get the matrix
    inv <- solve(data, ...)  # Compute the inverse
    x$setInverse(inv)  # Cache the inverse
    
    # Return the inverse
    inv
}
