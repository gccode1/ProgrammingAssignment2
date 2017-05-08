makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    getInv <- function() inv
    setInv <- function(y) inv <<- y
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data = x$get()
    inv = solve(data, ...)
    x$setInv(inv)
    inv
}