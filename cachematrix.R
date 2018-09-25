makeCacheMatrix <- function(x = matrix()) {
    m <- NULL 
    set <- function (y) {   # set values to the x to be calculated and reset inverse
        x <<- y
        m <<- NULL
    }
    get <-function() x  # returns the value of x as a function to be calculated in 'cacheSolve'
    setmatrix <- function(solve) m<<- solve     # the inverse calculated is stored in 'm' via this function
    getmatrix <- function() m    # simply gives the calculated inverse 'm
    list (set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)  # allows you to call functions with the $ operator
}
cacheSolve <- function(x, ...) {   # does not take in atomic vector; it needs a function to inititate
    m <- x$getmatrix()    # get m to its value to run through the if statement
    if(!is.null(m)){      # checks if m has already been calculated
        message("getting the cached inverse")
        return(m)         # returns if it already has been calculated; from cache
    }
    data <- x$get()
    m <- solve(data,...)    # calculates the inverse using the 'solve' function
    x$setmatrix(m)          # assigns the new inverse to 'm' via the setmatrix function to be stored in cache
    m
    ## Return a matrix that is the inverse of 'x'
}