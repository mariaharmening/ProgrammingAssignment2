#the following functions will cache potentially time-consuming computations.  Below are two functions
#that are used to create a special object that stores a matrix and caches its inverse

## makeCacheMatrix creates a matrix, which is really a list containing a function to 
#set the value of the matrix,
#get the value of the matrix, 
#set the value of the inverse, 
#and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }  
    get <- function() x 
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix created with the above function.  it first checks to see if the inverse
#has already been calcuated.  if so, it get the inverse from teh chase and skips the computation.  Otherwise,
#it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) 
{
      m <- x$getinverse()
      if(!is.null(m))
      {
          message("getting cached data")
          return(m)
      
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
