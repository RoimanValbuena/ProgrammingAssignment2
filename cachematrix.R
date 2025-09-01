# makeCacheMatrix: Crea un objeto especial que almacena una matriz y su inversa en caché
makeCacheMatrix <- function(x = matrix()) {
  m <- list()
  
  m$setMatrix <- function(value) {
    x <<- value
    m$inverse <<- NULL
  }
  
  m$getMatrix <- function() {
    return(x)
  }
  
  m$setInverse <- function(value) {
    m$inverse <<- value
  }
  
  m$getInverse <- function() {
    return(m$inverse)
  }
  
  return(m)
}

# cacheSolve: Calcula la inversa de la matriz almacenada en el objeto creado por makeCacheMatrix
cacheSolve <- function(x, ...) {
  if (!is.null(x$getInverse())) {
    return(x$getInverse())
  }
  
  matrix <- x$getMatrix()
  inverse <- solve(matrix, ...)
  
  x$setInverse(inverse)
  
  return(inverse)
}