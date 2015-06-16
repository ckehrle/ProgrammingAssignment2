## Put comments here that give an overall description of what your
## functions do
## Used Sources:
##  https://github.com/rdpeng/ProgrammingAssignment2/introduction
##  http://www.statmethods.net/advstats/matrix.html

## Write a short comment describing this function
## makeCacheMatrix takes a matrix as input an returns a list structure
## implementing a getter/setter object structure.
## the functions get and set functions are used to save and return the 
## original matrix. the functions set_inverse and get_inverse are used
## to calculate and store the inverse matrix and to return the stored 
## inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
    matrix <- NULL
    # if a new marix is stored, it will be checked if new matrix is identical with
    # with old matrix, if so an eventually stored inverse matrix is preserved.
    set <- function(y) {
        if (!identical(x,y)){
            matrix <<- NULL
        } 
        x <<- y
    }
    ## return original matrix
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
## cacheSolve uses the list structure creates in function makeCacheMatrix.
## It is checked if the inverse matrix is already stored in structure.
## If it is already stored , the stored value is returned.
## If not the value is calculated , stored and then returned.
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
    ## store value in data structure and return value
    x$set_inverse(inverse_matrix)
    inverse_matrix
}
