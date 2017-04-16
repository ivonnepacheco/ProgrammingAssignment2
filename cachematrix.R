## R assigment week 2;
## The functions will create a special matrix with the elements needed 
## to store in chache the matrix and its inverse, and retrieve the value 
## from the cache if it exists to avoid recalculations and reduce ejectution time.

## makeCacheMatrix will create a list of elemnts consisting in:


makeCacheMatrix <- function(TargetMatrix = matrix()) {
  ## Creates the variable inverse, initially to NULL.
  Inverse <- NULL 
  ## set/update the value of the matrix and set to NULL its inverse.
  set <- function(y){
    TargetMatrix <<- y
    Inverse <<- NULL
  }
  ## Print the value of the Matrix
  get <- function() TargetMatrix
  ## Calculate the value of the variable inv with the Inverse of the TargerMatrix
  setInverse <- function(Inv) Inverse <<- Inv
  ## Print the inverse stored value
  getInverse <- function() Inverse
  ## Create a list of the 4 created elements
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Using the list of elements created with makeCacheMatrix calculate the inverse if not calculated before.

cacheSolve <- function(x, ...) {
  ## Take the Inverse from the list x
  Inverse <- x$getInverse()
  ## Check if it was alrealy calculated. If so, return the value from the cache, else calculate it.
  if (!is.null(Inverse)) {
    message("getting cached data")
    return(Inverse)
  }
  mat <- x$get()
  Inverse <- solve(mat, ...)
  x$setInverse(Inverse)
  Inverse
}

## Using the functions:
## Matrix to invert:
x <- matrix(5:8, 2)

functions <- makeCacheMatrix()  ## Create a variable storing the results of the function, in this case 4: 
                                ## set, get, setInverse, getInverse
functions$set(x)                ## Internally the matrix has been defined in functions
functions$get()                 ## This one displays the matrix created with set
cacheSolve(functions)           ## Calculates the inverse because it has not been calculated
x%*%cacheSolve(functions)       ## Uses the value of the cache to perform the calculation
