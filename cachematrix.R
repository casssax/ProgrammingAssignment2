## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
