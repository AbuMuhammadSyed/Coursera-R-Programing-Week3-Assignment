## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(a = matrix()) {## Function for new a "matrix" object that will cache the matrix inverse
  ## defining the argument with default mode of "matrix"
  inv <- NULL                ## initializing inv as NULL; will hold the inverse matrix value                   
  set <- function(b) {      ## defining the set function to assign new              
    a <<- b                 ## value of matrix in parent environment       
    inv <<- NULL            ## if there is a new matrix, resetting inv to NULL   
    }
  get <- function() a       ## defining the get fucntion - which returns value of the matrix argument
  
  setinverse <- function(inverse) inv <<- inverse ## assigning value of inv in parent environment 
  getinverse <- function() inv              ## gets the value of the inv where called       
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## we need this in order to referto the functions with the $ operator
} 




## Write a short comment describing this function
## This function will computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse calculated (and the matrix has not changed),
## then cacheSolve will take the inverse from the cache
cacheSolve <- function(a, ...) {
        ## Return a matria that is the inverse of 'a'
  inv <- a$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- a$get()
  inv <- solve(data, ...)
  a$setinverse(inv)
  inv
}
