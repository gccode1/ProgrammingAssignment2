



## This function make a cache of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv = NULL

    # set function is not needed, I don't know why is it given in the assignment (while mean calculation)
    # set <- function(y){
    #     x <<- y
    #     inv <<- NULL
    # }

    get <- function() x   # to get the matrix
    getInv <- function() inv   # to get inverse of the matrix
    setInv <- function(y) inv <<- y   # to set inverse of the matrix, where y is the inverse
    list(get = get,          #  make a list of all the function, so that is can be accessed from outside
         setInv = setInv,
         getInv = getInv)
}


cacheSolve <- function(x, ...) {
    inv <- x$getInv()        # get inverse of the matrix
    if(!is.null(inv)){       # if inv is not null, then return inverse from the cache
        message("getting cached data")
        return(inv)
    }
    data = x$get() # get actual matrix from makeCacheMatrix object. 
    inv = solve(data, ...)    # solve function will return the inverse of data
    x$setInv(inv)             # set inverse so that we can get it from cache
    inv                       # return inverse
}
