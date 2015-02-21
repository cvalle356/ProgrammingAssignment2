## Creates a two functions. the first function creates a list of four funtions, 
##  accepts a matrix (within function), and sets value for the inverse of the funtion (within function).
## The second function checks the list created by first function to see if an inverse 
##    matrix has been calculated ad set, then calculates and caches the inverted 
##    matrix using the appropriate functions in the list. If an inverted matrix exist, 
##    it will return the cached value and a message stating it has access cached rather than calculating
##

## This function creates a list of seperate functions and accepts a matrix values(within function). 
## It also creates a place holder for the calculated inverse matrix (within function)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)

}


## This function utilizes the list containing 4 functions made by makeCacheMatrix and values
##    (matrix and space for inverted matrix) passed to those functions. 
## It checks to see if the inverse matrix of the value from the makeCacheMatrix has been calculated and saved.
## If the value is null get the value(matrix), inverse the matrix and then cache the value by binding the calculated
## value to the designated variable in the related function. It uses the appropriate functions in the list. 

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        
                m <- x$getmatrix()
                if(!is.null(m)) {
                        message("getting cached matrix")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setmatrix(m)
                m
        
}
