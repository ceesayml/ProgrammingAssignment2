## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
