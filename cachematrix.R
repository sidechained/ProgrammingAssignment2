# This 'cachematrix.R' file consists of two functions, 'makeCachematrix' and 'cacheSolve', where:
# - 'makeCachematrix' sets up an interface for setting and getting a matrix and it's inverse
# - 'cacheSolve' checks if a previous calculated inverse matrix already exists. If so, it returns this, if not the inverse of the matrix is calculated for the first time and stored. For large matrices this caching process can save time and processing power.

# First here is the 'makeCacheMatrix' function. This is similar to the 'makeVector' example, except:
# - instead of the m variable, I used s (for 'solve')
# - instead of setmean and getmean, I used setinverse and getinverse
# - in setinverse, solve is called, not mean

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Now for the 'cacheSolve' function. This is the same as the 'cachemean' example, except:
# - m has been replaced by s (again to represent 'solve')
# - getmean() has been replaced by getinverse()
# - 'mean' function has been replaced by 'solve'

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}

# Now for a simple test. The code below generates a test matrix, then calls the 'makeCacheMatrix' function with the test matrix to set up the interface. Then the 'cacheSolve' function is called and the matrix inverse is calculated. When 'cacheSolve' is called a second time, the already stored inverse is retrieved. Please uncomment the lines below to try this out (don't forget to run the above two functions first):

#A <- matrix(c(3, 2, 5, 2, 3, 2, 5, 2, 4), nrow = 3, ncol = 3)
#x = makeCacheMatrix(A)
#cacheSolve(x) # the second time the previous stored matrix is retrieved
#cacheSolve(x) # the first time the inverse is calculated