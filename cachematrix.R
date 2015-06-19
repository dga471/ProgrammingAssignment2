## Similar to the makeVector() function in the test example, makeCacheMatrix takes a single argument "x", the matrix which we want to invert, and returns a list of 4 functions: set, get, setinv, and getinv

#Note that inv is the inverse of the matrix - the thing we are ultimately trying to find. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # sets inv to NULL
  set <- function(y){  #defines the function set(), which receives a single argument y and sets x to the value of y. Also sets inv to NULL again.
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x #defines the function get(), which simply returns the matrix x
  
  setinv <- function(solve) inv <<- solve #defines the function setinv, which takes in a single argument solve (the inverse of the matrix) and sets the value of inv to solve, storing it in the cache.
  
  getinv <- function() inv #defines the function getinv, which simply returns the inv in the cache (if any exists)
  
  list (set=set, get=get, setinv=setinv, getinv=getinv) #this puts everything into a list which is returned
}


## cacheSolve is a function that takes in an argument funcvec, which is the list of functions produced by makeCachematrix() above. It then returns inv, the inverse of the matrix we put into makeCachematrix().

cacheSolve <- function(funcvec, ...) {
  inv <- funcvec$getinv() #first, obtains inv using the function defined in makeCachematrix()s
  
  if(!is.null(inv)) { #checks if inv has already been calculated and exists in the cache
    message("getting cached data")
    return(inv) #if it has been, it simply returns it without having to recalculate it.
  }
  
  #if the inverse is not found in the cache, then we have to calculate it.
  data <- funcvec$get() #retrieves the matrix from funcvec
  inv <- solve(data,...) #calculates the inverse and sets inv to it.
  funcvec$setinv(inv) #sets the inverse to the cache by calling the function setinv in funcvec, such that we won't need to calculate it again in the future.
  inv #returns the inverse
}
