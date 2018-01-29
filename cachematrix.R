## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 1- Take a matrix as argument
## 2- Set the value of the matrix
## 3- Retrieve (get) the value of the matrix
## 4- Set the value of the inverse of the matrix
## 5- Get the value of the inversed matrix
## 6- Return a list object with the functions to the parent environment

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the object inversed_matrix with value NULL
    inversed_matrix <- NULL
    
    ## Creates the set() function that does two tasks:
    ## 1- assign the input argument to the x object in the parent environment
    ## 2- assign the value of NULL to the inversed_matrix object in the parent environment
    set <- function(y) {
        x <<- y
        inversed_matrix <<- NULL
    }
    
    ## Creates the get() function to define the value of the matrix x
    get <- function() x
    
    ## Calculates the inverse of matrix using solve() function
    setinverse <- function(solve) inversed_matrix <<- solve
    
    ## Retrieves the inversed matrix value
    getinverse <- function() inversed_matrix
    
    ## Creates a list object and returns it to be used in cacheSolve()
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## Populate and retrieve the inverse of a matrix from 
## an object type makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ## Retrieve the inversed_matrix received as argument
    inversed_matrix <- x$getinverse()
    
    ## Checks if the inversed_matrix is NULL
    ## If inversed_matrix not null return it to the parent environment 
    if(!is.null(inversed_matrix)) {
        message("getting cached data")
        return(inversed_matrix)
    }
    ## if inversed_matrix is null gets the matrix from the input object
    data <- x$get()
    ## calculates the inverse of matrix using solve() function
    inversed_matrix <- solve(data, ...)
    ## uses the setinverse() function on the input object
    x$setinverse(inversed_matrix)
    ## return the value of the inversed matrix
    inversed_matrix
}
