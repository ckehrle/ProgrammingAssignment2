## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    matrix <- NULL
    set <- function(y) {
        x <<- y
        matrix <<- NULL
    }
    ## return original martrix
    get <- function() x
    ##store inverse matrix variable matrix using solve function which is defined in other environement
    set_inverse <- function(solve) matrix <<- solve
    ## return stored values.
    get_inverse <- function() matrix
    list(set = set, 
         get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

## Write a short comment describing this function
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## We retrieve the inverse matrix from datastructure 'x'
    inverse_matrix <- x$get_inv()
    ## in case there is a value stored we reply the value stored
    if(!is.null(inverse_matrix)) {
        message("getting cached data")
        return(inverse_matrix)
    }
    ## if no value is set then we calculate the inverse matrix
    ## assign datastructure
    data <- x$get()
    ## use solve on data structure
    inverse_matrix <- solve(data, ...)
    #store value in data structure and return value
    x$set_inverse(inverse_matrix)
    inverse_matrix
}
