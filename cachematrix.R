###########################

## cachematrix.R is a pair of functions:
## 'makeCacheMatrix', and 'cacheSolve'
## that demonstrate creating a list object
## that points to defined functions, data caching,
## and exercising the list object by inverting a matrix

###########################

# makeCacheMatrix is a function that
# generates a list object containing functions to
#
#  set the value of the vector
#  get the value of the vector
#  set the value of the mean
#  get the value of the mean

# makeCacheMatrix is intialized with by passing a matrix object

# note the use of '<<-" where ".. cause[s] a search to made through parent 
# environments for an existing definition of the variable being assigned
# If such a variable is found (and its binding is not locked) then its
# value is redefined, otherwise assignment 
# takes place in the global environment..."

makeCacheMatrix <- function(x = matrix()) {
  
  #direct call of makeCacheMatrix stores matrix x and sets
  # set inverse matrix as null
  
  inverse <- NULL
  
  # 'set' stores a new matrix and sets inverse matrix to null
  # This is similiar direct calling makeCacheMatrix for
  # the first time, but allows resetting of an already existant
  # makeCacheMatrix object
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # return stored matrix
  get <- function() x
  
  # store inverse matrix
  setInverse <- function(solvedInverse) inverse <<- solvedInverse
  
  # return inverse matrix
  getInverse <- function() inverse
  
  # enumarate list functions
  list( set=set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

###########################

# cacheSolve returns a matrix that is the inverse of 'x'
# 'x' is an list object returned from function makeCacheMatrix
# 'x' *must* must initialized by passing makeCacheMatrix a matrix object
# for cacheSolve to work

cacheSolve <- function(x, ...) {
  
  ## Retreive value for x$getInverse()  
  
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) {
    # if value x$getInverse() exists, 
    # inverse cached, then return value
    message("getting cached data")
    return(inverse)
  } else {
    # value x$getInverse() does not exist
    # (inverse not cached)
    # so solve matrix retrieved by 
    # x$get() and store inverse with x$setInverse()
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
    inverse
  }
  
}

###########################

# Test code for makeCacheMatrix and cacheSolve

# n=2
# set.seed(1)
# x<-matrix(runif(n^2),n) # Create n x n matrix w/ uniform distrubuted  random numbers
# x # show matrix x
# y<-solve(x) #invert x outside of makeCacheMatrix/cacheSolve
# y # show inverted matrix
# z<-makeCacheMatrix(x) # initialize makeCacheMatrix object Z
# cacheSolve(z) # first round return inverse z and cache inverse
# cacheSolve(z) # retrieve cached inverse z