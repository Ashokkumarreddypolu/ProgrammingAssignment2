## Taking Invertible matrix as input and storing in catch. 
## Display inverse of matix after calculation or from catch if exists 
# Assumed input matrix is Invertible

makeCacheMatrix <- function(x = matrix()) {
  
  # stores the cached value
  # initialize to NULL
  m <- NULL
  
  # create the matrix in the working environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  # invert the matrix and store in cache
  setMatrix <- function(inverse) m <<- inverse
  # get the inverted matrix from cache
  getInverse <- function() m
  
  # return the created functions to the working environment
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
  
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  
  m <- x$getInverse()
  
  # return inverted matrix from cache if it exists
  # else create the matrix in working environment
  if (!is.null(m)) {
    message("getting cached data")
    
    # display matrix in console
    return(m)
  }
  
  # create matrix since it does not exist
  matrix <- x$get()
  
  # make sure matrix is square and invertible
  # if not, handle exception cleanly

    # calculate inverse of matrix
    m <- solve(matrix, ...)

    # set inverted matrix in cache
    x$setMatrix(m)

  
  # display matrix in console
  return (m)
        ## Return a matrix that is the inverse of 'x'
}
