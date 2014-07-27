## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function, makeVector creates a special "vector", which is really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL  # the inverse start
 
 set <- function(y ) {
            x <<- y
            inv <<- NULL
    } # set the matrix
    
 get <- function() { x } # Return the matrix
 
 setinverse <- function(inverse) { inv <<- inverse } # Set the inverse matrix

 getinverse <- function() { inv } # Retern the inverser matrix

 list(set = set, get = get,
      setinverse=setinverse,
      getinverse=getinverse) # Return a list

}



## Write a short comment describing this function
# The following function computes the inverse of the  matrix returned by "makeCacheMatrix"
# It checks if the inverse has already been computed. If so, then return the result and 
# skips the computation. If not, computes the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()

        if(!is.null(inv)) {
                message("getting cached data")
                return(inv) # return the inverse if already exist
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv ## Return the matrix
}


##Test run
# x = matrix(3:6, 2,2)
# m = makeCacheMatrix(x)
# m$get()
# cacheSolve(m) 
# cacheSolve(m)

        

