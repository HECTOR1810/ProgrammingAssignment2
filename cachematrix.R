## The first function created a set of functions we can use to get
## and/or set a cache value for the inverse of a given 'x' matrix

## makeCacheMatrix will create four functions that will help us 
## store information of the argument x, it sets the base to later
## use cacheSolve.These functions help us store and retrieved cached
## inverse values for our matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## set  is like restarting the values, setting again the values.
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  ## get just retrieves the value of the argument x.
  get <- function() {x}
  ## This will help later when we  want to assign a value to the
  ## inv variable we set to hold the inverse. In this case inverse
  ## will be the inverse function we manage  to solve for latter
  setinv <- function(inverse) {inv <<- inverse}
  ## getinv retrieves the value 
  getinv <- function() {inv}
  ## Returning this list helps us preserve the functions we created
  ## for the argument x.
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function will look for the value of the inverse, if the
## value is already cached it will returned the value. If it's not
## stored already it will get its inverse and store it in the cache.

cacheSolve <- function(x, ...) {
  ## This will get the value, the default value for the inverse
  ## will be NULL it it hasn't been cached.
  inv <- x$getinv()
  ## Checks if it already has a value different than NULL and
  ## returns it if it does exist. 
  if(!is.null(inv)){return(inv)}
  matrix <- x$get()
  ## Since the argument b is not given, b will be an identity
  ## matrix and solve will return the inverse of our matrix.
  inv <- solve(matrix,...)
  ## We set the cache value
  x$setinv(inv)
  ## Returns the inverse 
  inv
}
