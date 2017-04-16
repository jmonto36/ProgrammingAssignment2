#Assigment Alejandro Montoya

#First function assigment

makeCacheMatrix <- function(m = matrix()) {
  m_inverse <- NULL
  set <- function(y) {
    m <<- y
    m_inverse <<- NULL
  }
  get <- function() m
  setinverse <- function(solve) m_inverse <<- solve
  getinverse <- function() m_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#Second function assigment

cacheSolve <- function(m, ...) {
  m_inverse <- m$getinverse()
  if(!is.null(m_inverse)) {
    print("getting cached data")
    return(m_inverse)
  }
  data <- m$get()
  m_inverse <- solve(data, ...)
  m$setinverse(m_inverse)
  m_inverse
}
