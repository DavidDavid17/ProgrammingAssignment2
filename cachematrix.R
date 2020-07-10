## This assignment is about creating an R function which will be able to cache potentially 
## time-consuming computations. For example, taking the inverse of a 2x2 matrix will not take a long time.
## However, for 5x5 matrix or above, it may take too long to compute the inverse, especially if it 
## has to be computed repeatedly (for example: in a loop). If the contents of a matrix are not changing,
## it may make sense to cache the value of the inverse so that when we need it again, it can be looked up 
## in the cache rather than recomputed. 

## The function "makeCacheMatrix" creates a special matrix, which is basically a list containing 
## a function to set the value of the matrix, get the value of the matrix, set the value of the inverse 
## and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the function above. If the inverse has already 
## been calculated, it will grab the inverse from the cache instead of calculating it all over again. 


cacheSolve <- function(x, ...) {
  inverse  <- x$getinverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse     
}
## Return a matrix that is the inverse of 'x'


matrix <- makeCacheMatrix(             ## We first set the makeCacheMatrix function to the variable matrix
  matrix(c(1:4),2,2)                   ## We set the matrix to have range from 1 to 4 in a 2x2 matrix
)                                    
matrix$get()                          

cacheSolve(matrix)                     ## We grab the cache since the inverse has already been calculated

matrix$getinverse()                    ## We compute the inverse to make sure that the result is the same 
                                       ## with the our previous cache

matrix <- makeCacheMatrix(            
  matrix(c(1,0,-3,-5,8,2,-1,-7,9),3,3) ## We set up another 3x3 matrix with random numbers.
)
matrix$get()

cacheSolve(matrix)                     ## We grab the cache since the inverse has already been calculated

matrix$getinverse()                    ## We compute the inverse to make sure that the result is the same 
                                       ## with the our previous cache

