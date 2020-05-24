## Together, this pair of functions returns the inverse of a square invertible matrix.
## By enabling retrieval of a cached value, you avoid wasting computational power.

## This function creates a list of functions that enables storage and retrieval of a cached matrix. It also creates the objects x and inv.

makeCacheMatrix <- function(x = matrix()) { ## initializes object x as the input matrix
    inv <- NULL
    ## initializes the value of inverse matrix to NULL
    set <- function(y) { 
        x <<- y
        ## searches parent evironment for x and resets it to y (the input matrix)
        inv <<- NULL 
        ## searches parent environment for inv and redefines it to NULL
        ## prevents retrieving values from previous makeCacheMatrix calls
    }
    get <- function() x ## returns the matrix that was input to makeCacheMatrix
    setinv <- function(inverse) inv <<- inverse
        ## searches parent environment for inv and redefines it to the new inverse matrix.
        ## Note this does NOT redefine inv until it is called
        ## It is only called in cacheSolve if getinv returns NULL
    getinv <- function() inv ## returns inv if it exists or NULL if not yet cached
        ## It might be more clear to place this before setinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(LoFs, ...) {
        ## Return a matrix that is the inverse of 'x' 
        ## Using x again here is confusing because the x argument to cacheSolve has to be a list of the type output by makeCacheMatrix so I changed it to LoFs for List of Functions
    inv <- LoFs$getinv() 
        ##assigns inv the value returned by the function getinv() defined above
        ##this will be the cached inverse matrix if it has previously been calculated
    if(!is.null(inv)) {  ##if a cached object was found, return it and terminate
        message("getting cached data")
        return(inv)
    }
    data <- LoFs$get() ##if a cached object was not found, set data to the matrix input to makeCacheMatrix
    inv <- solve(data, ...)  ## calculate the inverse of the matrix and assign it to inv
    LoFs$setinv(inv) ## assign the newly calculated inverse matrix to inv in the parent environment - this means it will be found there as a cached object if function called again
    inv ##return the inverse matrix
}
