## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## set x and m and assign it to parent envirement then get x value
## set inverse matrix then get the inverse matrix
## list setters and getters

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }  
        get <- function() x
        set_inverse_matrix <- function(inverse_matrix) m <<- inverse_matrix
        get_inverse_matrix <- function() m
        list(set = set, get = get,
             set_inverse_matrix = set_inverse_matrix,
             get_inverse_matrix = get_inverse_matrix)
        
}


## Write a short comment describing this function
## cacheSolve will take the matrix from makeCacheMatrix and check if the inverse matrix calculated before or not
## if it was calculated, it will take the inverse matrix from the memory and print a massage
## if not it will be calculted and saved in cacheSolve
## ginv function to calclute the inverse matrix from MASS package

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        library(MASS)
        m <- x$get_inverse_matrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- ginv(data, ...)
        x$set_inverse_matrix(m)
        m
}
