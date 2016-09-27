## If we are going to call a same function multiple times where the arguments to 
## the function hasn't changed then it is good to cache the result into an 
## object and call the object instead of computing the value again and again.
## This will reduce the process time for the fucntoin. Below we are creating a 
## function which will generate a inverse of a invertablematrix and assign it to 
## an object if the argument hasn’t change.

## creating a function called makeCacheMatrix which creates a special matrix 
## object that can store it inverse of the Matrix.

makeCacheMatrix <- function(x = matrix()) {
  ## creating an object inversematrix and assigning Null to it.
  inversematrix <- NULL
  
  ## creating a function called set to assign the value y (which is the argument
  ## to the function set) to x and assigning the object inversematrix to null
  set <- function(y) {
    x <<- y
    inversematrix <<- NULL
  }
  
  ## creating an anonymous function which will get the value of x which is
  ## the matrix set by the above function.
  get <- function()
    x
  
  ## creating a function to set the object inversematrix as solve
  ## which is the argument passed to this function and assigning it
  ## to a object setimatrix
  setimatrix <- function(solve)
    inversematrix <<- solve
  
  ## creating a anonymous function to set the object inversematrix which is
  ## set buy the above function which is actually the inverse of the
  ## matrix
  
  getimatrix <- function()
    inversematrix
  
  ## creating a list with all the functions created under the object
  ## makeCacheMatrix
  list(
    set = set,
    get = get,
    setimatrix = setimatrix,
    getimatrix = getimatrix
  )
}


## This fucntion checks if the inverse of the matrix is already created, if so
## it will return the inverse matrix from the cache.If the inverse is not created
## then it will create the inverse of the matrix and assign it to the cache and
#x return the newly generated inverse matrix.

cacheSolve <- function(x, ...) {
  
  ##fetching the inverse matrix which is stored in getimatrix
  inversematrix <- x$getimatrix()
  
  ##checking if the getimatrix is not null, it is not not then
  ## it will return in the inverse from the cached data
  if (!is.null(inversematrix)) {
    message("getting cached data")
    return(inversematrix)
  }
  
  ##If the getimatrix is null then we are getting the matrix value from
  ##get object and storing it in the object called data
  data <- x$get()
  
  ## calculatin the inverse of the matrix using solve functoin
  inversematrix <- solve(data, ...)
  
  ##assigning to the calculated inverse to setimatrix object in the
  ##makeCacheMatrix fucntion.
  x$setimatrix(inversematrix)
  
  ##returning the calculated inversematrix
  inversematrix
}
