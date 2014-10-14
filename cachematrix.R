## Caching the Inverse of a Matrix
## makeCacheMatrix: Caches the matrix and cachesolve: Computes / Gets the inverse from cache

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function (x = matrix()) {
  inv <- matrix()
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inv2) inv <<- inv2
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## cacheSolve: This function calculates the inverse of a matrix
## If inverse of the matrix has already been calculated (and the matrix has not changed), get the inverse from cache and skip computation
## Otherwise, calculate the inverse of the matrix and set the value of inverse in cache

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  m <- x$get()
  epsilon <- 1e-6
  #inv has the result of invocation to getInvx() function of makeCacheMatrix
  #This makes: class(x) a "list"
  if(!is.na(inv[[1]])) {
    message("getting cached data")
    return(inv)
  }
  #Otherwise, get data, calculate inverse, and set the inverse
  else if (abs(det(m))<epsilon) {
    message("Non invertible Matrix: System is exactly singular")
  } else {
    inv <- solve(m)
    x$setInv(inv)
    return(inv)
  } 
}

##Testing: l <- makeCacheMatrix(matrix(c(2,3,2,3), nrow=2)); cacheSolve(l) #Singular Matrix
##Testing: li <- makeCacheMatrix(matrix(c(2,3,2,2), nrow=2)); cacheSolve(li) #Normal output
