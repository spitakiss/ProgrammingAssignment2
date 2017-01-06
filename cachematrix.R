## Pair of functions to return inverse of square matrix
## Uses cached inverse calculation, if available
## Functions follow general form described by Roger Peng @ https://github.com/rdpeng/ProgrammingAssignment2


## Function creating matrix object that can cache inverse of square matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
    
}


## Function to calculate inverse of matrix.  Use cached solution, if available

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)){
        message("getting cached inverse!")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setInv(inv)
    inv
}



# Insert your testing code below
a <- matrix(1:4,2,2)
b <- makeCacheMatrix(a)


cacheSolve(b)
cacheSolve(b)
