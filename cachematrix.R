## Coursera Assignment 2

#Always use braces, no matter the number lines in the iteration or function
#Method to Cache an inverted matrix
makeCacheMatrix <- function(x = matrix()) 
{
  #Initialize inverted matrix param
  iX <- NULL

  #Setter for original matrix
  set <- function(y) 
  {
    x <<- y
    iX <<- NULL
  }
  #Getter for original matrix
  get <- function()
  { 
    return(x)
  }
  
  #Setter for inverted matrix
  setInvertedMatrix <- function(inverse)
  {
    iX <<- inverse
  }

  #Getter for inverted matrix
  getInvertedMatrix <- function()
  { 
    return(iX )
  }
  
  #Keep a list of getters and setters
  list(set = set, 
       get = get,
       setInvertedMatrix = setInvertedMatrix,
       getInvertedMatrix  = getInvertedMatrix
      )
}

#Method computes the inverse of a matrix and stores in a cache
#If the inverted matrix exists in the cache it returns the value without computation
cacheSolve <- function(x, ...) 
{
  #check cache
  iX <- x$getInvertedMatrix()
  if(is.null(iX))
  {
    #cached matrix is available
    message("inverted matrix from cache")
    return(iX)
  }  
  #compute inverted matrix
  data <- x$get()
  iX <- solve(data)
  
  #set in cache for future use 
  x$setInvertedMatrix(iX)

  return(iX)
}




