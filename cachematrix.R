## https://github.com/safkat93/ProgrammingAssignment2.git
## My first SHA-1 hash commit: d60970e03e5c45bfc428673f1cf0cdbeb9d1674d
## Week 3 Assignment : Lexical Scoping 

## New special function "matrix" has been created to cache makeCacheMatrix inverse
## Following list contains
## insert the matrix value 
## obtain the matrix value 
## insert the inverse value of the matrix 
## obtain the inverse value of the matrix
makeCacheMatrix <- function(x = matrix()) {
    n <- NULL
    set <- function(y) {
        x <<- y
        n <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) n <<- inverse
    getinverse <- function() n
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Following function is used to calculate the special "matrix"
## Before the function starts its calculate it confirms the previous calculated inverse 
## The computation is skipped if the inverse is recieved from the previous function
## If not, the inverse in calculated to set the inverse value 
## Setinverse in finally obtained

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    n <- x$getinverse()
    if(!is.null(n)) {
        message("getting cached data")
        return(n)
    }
    data <- x$get()
    n <- solve(data, ...)
    x$setinverse(n)
    n
}
