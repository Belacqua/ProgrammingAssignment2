## Storing a Matrix and Cashing its Inverse
## David R Scott (Belaqua @me.com) 
##Programming Assgnment 2 (from Week 3) -- Data Science Specialization -- R Programming (3/2016)

##FUNCTION 1 (of 2)
##This function, "makeCacheMatrix", creates a special "matrix" which is really a list containing a function to
##do the following: 
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the matrix inverse
##4. get the value of the matrix inverse

makeCacheMatrix  <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#END FUNCTION 1
##########################################

##FUNCTION 2 (of 2)
##This function checks to see if the inverse of a  matrix has already been calculated. 
##If so, it  gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the matrix and sets the value of the inverse matrix in the cache 
##via the  setinverse  function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
#END FUNCTION 2

