makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setni <- function(ni) m <<- ni
    getni <- function() m
    list(set = set, get = get,
         setni = setni,
         getni = getni)
}


cacheSolve <- function(x, ...) {
    m <- x$getni()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setni(m)
    m
}