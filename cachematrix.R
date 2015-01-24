
#The functions makeCacheMatrix() and cacheSolve() are used together to cahce a matrix
#and calculate and cache it's inverse


#This function takes a matrix as input, stores it, and returns a list of functions and their
#definitions which will then be used to get/set the value of the matrix, and it's inverse
makeCacheMatrix <- function(x = matrix())  {
  
  #initailize m to null
  m <- NULL
  
  set <- function(y)  {
    #set x to new matrix
    x <<- y
    
    #clear cached inverse value if there is one by setting to null
    m <<- NULL
    
  }
  
  #return the current matrix stored in x
  get <- function() x
  
  #set m to be the calculated inverse value for the matrix x
  set_inverse <- function(inverse_matrix) m <<- inverse_matrix
  
  #return m (will be null if there is no value)
  get_inverse <- function() m
  
  #return the list of functions and their definitions
  list (set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
  
  
}

#This function takes a matrix created by the makeCacheMatrix function as an argument
#and calls the get_inverse() function to retrieve the currently
#cached value of 'm' (inverse matrix), if there is one
cacheSolve <- function(x, ...) {
  
  #get cached value for the inverse
  cached_inverse <- x$get_inverse()
  
  #if there is a value currently stored in 'm' return it
  if(!is.null(cached_inverse)) {
    message("getting cached data")
    return(cached_inverse)
  }
  
  #else, get the matrix that is currently saved and calculate it's inverse using the solve()
  #function
  data <- x$get()
  cached_inverse <- solve(data, ...)
  
  #call the set_inverse() function to cache the calculated value for the inverse
  x$set_inverse(cached_inverse)
  #return the inverse
  cached_inverse
}