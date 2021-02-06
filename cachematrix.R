## The first function makeCacheMatrix creates a special matrix which is really a 
## list to: set the value of the matrix; get the value of the matrix; set the 
## inverse of the matrix; and get the value of the inverse.
## and the "cacheSolve" function works in conjunction with the makeCacheMatrix
## and it computes the inverse of the matrix. But it first checks if the inverse 
## has been cached already, and if so it skips the computation and brings the 
## cached data. if it hasnt been cached, the function calculates and caches
## it via the setInverse() function



## this first function takes an invertible matrix as argument x
## The first action of this function is to create an empty variable where the
## inverse of the matrix will be stored when it is calculated.
## then we have an interior function that sets the value of the matrix x when you
## wish to change it. And when you change the value with the interior set function
## the cached inverse gets reset to NULL
## then the next interior function just gets the value of the matrix
## the next one sets the value of the inverse and caches it
## and the last one displays the inverse if it has been cached, or NULL if it hasn't

makeCacheMatrix <- function(x = matrix()) {
    the_inv <- NULL                   

    set <- function(y){
        x <<- y                   
        the_inv <<- NULL
    }
    get <- function(){x}  
    setInverse <- function(inverse){ the_inv <<- inverse}
    getInverse <- function(){ the_inv }
    
    list( get = get, set = set, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve matrix takes argument x, which is the previous function ( you 
## should assign it a name for simplicity like the "mymat" example below.
## the function first checks if the inverse of the matrix has already been cached
## if it has, the function skips the calculation and fetches the cached data
## if the inverse has not been cached, the function gets the value of the matrix
## and inverts it, and then caches the value of the inverse.
## examples are below.

cacheSolve <- function(x, ...) {
    the_inv <- x$getInverse()
    if(!is.null(the_inv)){
        message("It seems the inverse has already been cached, so i'll save myself
                the trouble of rigorous calculation, and just fetch the catched
                data. Here you go ;-) <3 <3")
        return(the_inv)
    } 
    data <- x$get()
    the_inv <- solve(data, ...)
    x$setInverse(the_inv)
    
    the_inv  ## Return a matrix that is the inverse of 'x'
       
}

mymat <- makeCacheMatrix(matrix(1:4,2,2))
mymat$get()
mymat$getInverse()

cacheSolve(mymat)
mymat$getInverse()

cacheSolve(mymat)
