# custom matrix object that stores the inverted Matrix of the object once it has
# been calculated

makeCacheMatrix = function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() {x}
  getInv = function() inv
  setInv = function (solve) {
    inv <<- solve(x)
  }
  list(set = set, get = get,
    setInv = setInv, getInv = getInv
  )
  
}

## Either calculates the inverse matrix or retrieves it if it already exists

cacheSolve = function(x, ...) {
  m = x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data = x$get()
  m = solve(data, ...)
  x$setInv(m)
  m
}