# The following functions calculate the inverse of a matrix and saves it
# to the cache such that the next time the user attempts to calculate the
# matrix inverse, the previously saved value is returned instead of
# repeating the calculation.

# This function creates a special "matrix" object, which is really a list 
# containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix<-function(x = matrix()) {
    inv<-NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <-function(inverse) inv <<- inverse 
    getinverse <-function() inv 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

# The following function calculates the inverse of the special "matrix" created
# with the above function. However, it first checks to see if the inverse
# has already been caclulated. If so, it 'get's the inverse from the cache
# and skips the computation. Otherwise, it calculates the matrix inverse
# and sets the value of the inverse in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv<-solve(data,...)
    x$setinverse(inv)
    inv 
}
