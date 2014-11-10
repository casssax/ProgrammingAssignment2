## Two functions that allow you to cache the inverse of a matrix
## and return that cached inverse matrix if it exists, otherwise calculates
## the inverse.

## takes a square matrix and returns a list of functions for
## getting, setting, getting inverse, setting inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y){
                x <<- y
                im <<- NULL
        }
        get <- function() x
        set_im <- function(inv_m) im <<- inv_m
        get_im <- function() im
        list(set = set, get = get,
             set_im = set_im,
             get_im = get_im)

}


## calculates the inverse of a matrix. If inverse of matrix is
## already cached, returns cashed value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$get_im()
        if(!is.null(im)){
                message("getting cached matrix")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$set_im(im)
        im
}
