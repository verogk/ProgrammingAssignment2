#These two functions were made to work with big matrices, and their inverse. Used together, they save the matrix in cache memory
#and calculate the inverse if they are updated

## makeCacheMatrix
#This function creates a special matrix, which actually is a list with functions that??
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the matrix inverse
# 4. get the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { #this function is used when I want to assign a matrix
    x <<- y
    m <<- NULL 
  }
  get <- function() x  #this function gives me a value, in case it was previously assigned
  setinverse <- function(inverse) m <<- inverse #this function gives me the matrix inverse in case it was calculated and assigned by cacheSolve
  getinverse <- function() m #this fuction delivers the calculated matrix inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



#cacheSolve returns a matrix that is the inverse of 'x' 

cacheSolve <- function(x, ...) {
  m <- x$getinverse() #it looks for the special matrix list, and checks if the inverse had been calculated
  if(!is.null(m)) { #if it was calculated previously, it does nothing
    message("getting cached data")
    return(m)
  }
  data <- x$get() #it looks for the matrix to invert
  m <- solve(data) #it calculates the inverse of the matrix
  x$setinverse(m) #it assigns the calculated inverse matrix to the x object
  m
}
