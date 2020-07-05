make_cache_matrix <- function( h = matrix() ) {
  
  ## Initialize the inverse property
  contador <- NULL
  
  
  set <- function( matrix ) {
    h <<- matrix
    contador <<- NULL
  }
  
  ## obtener matriz
  get <- function() {
    ## Regresa la matriz
    h
  }
  
  setInverse <- function(inverse) {
    contador <<- inverse
  }
  
  ## obtener inversa de la matriz
  getInverse <- function() {
    ## Regresa la inversa
    contador
  }
  
  ## Regresa  una list 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
  
  h <- x$getInverse()
  
  
  if( !is.null(h) ) {
    message("obtener datos en cache")
    return(h)
  }
  
  ## Get la matriz origen del objeto
  data <- x$get()
  
  ## Calcula la inversa usando la multiplicacion matricial
  h <- solve(data) %*% data
  
  ## Set  para inversa para el objeto
  x$setInverse(h)
  
  ## Regresa la matriz
  h
}
