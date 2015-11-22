## Generate a matrix that caches inverse



#generate cached matrix, creating NULL for inverse value placeholder

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setMat <- function(solve) m <<- solve
    getMat <- function() m
    list(set = set, get = get,
         setMat = setMat,
         getMat = getMat)
}


## Write a short comment describing this function


cacheSolve <- function(x = matrix(), ...) {
    
    m <- x$getMat()
    if(!is.null(m)) { 
        message("getting cached data")
        return(m) #returns already cached inverse
    } 
    data <- x$get() #if value is  NULL, get the matrix
    m <- solve(data, ...) #calculate the inverse for matrix
    x$setMat(m) #sets the NULLs to the inverse value
    m # Returns a matrix that is the inverse of 'x'
}


