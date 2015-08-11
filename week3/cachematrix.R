## These functions allow the creation of a "special matrix"
## that can cache the value of its inverse matrix after being
## calculated once

#' Creates a "special matrix", by returning a list of functions that
#' operate on this matrix(get/set its value, get/set its inverse)
#'
#' @param x The matrix to create (optional)
makeCacheMatrix <- function(x = matrix())
{
    inv <- NULL
    
    # set and get methods for the matrix:
    set <- function(y)
    {
        if (!identical(x,y)) # we set an equal matrix, let's keep the inverse
            inv <<- NULL
        x <<- y
    }
    get <- function() x
    
    # set and get methods for the inverse matrix:
    setInv <- function(i) inv <<- i
    getInv <- function() inv
    
    # return value:
    list(set = set, get = get,
         setInv = setInv, getInv = getInv)
}


#' Calculates the inverse matrix of an invertable matrix created with 'makeCacheMatrix'
#'
#' @param x The list object provided by 'makeCacheMatrix'
#' @param ... Other parameters for 'solve' function
cacheSolve <- function(x, ...)
{
    inv <- x$getInv()
    
    if(!is.null(inv))
    {
        message("returning cached inverse")
        return(inv)
    }
    
    # Calculate the inverse:
    inv <- solve(x$get(), ...)
    x$setInv(inv)
    return(inv)
}
