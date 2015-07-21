#wrapper function for caching a matrix and its inverse
#argument baseMatrix is the initial matrix
makeCacheMatrix <- function(baseMatrix = matrix()){
  
  #inverse matrix is cached here
  cachedInverse <- NULL
  
  #set the base matrix
  setBaseMatrix <- function(newMatrix = matrix()){
    
    #use scope assignment to store base matrix in makeCacheMatrix environment
    baseMatrix <<- newMatrix
    
    #use scope assignment to delete any previously existing 
    #inverse matrix from makeCacheMatrix environment
    cachedInverse <<- NULL
  }
  
  #retrieve the base matrix
  getBaseMatrix <- function(){
    baseMatrix
  }
  
  #cache the inverse of the base matrix
  setInverseMatrix <- function(inverseMatrix = matrix()){
    cachedInverse <<- inverseMatrix
  }
  
  #retrieve the inverse matrix
  getInverseMatrix <- function(){
    cachedInverse
  }
  
  
  #expose functions as a list for easy access
  list(setBaseMatrix = setBaseMatrix, 
       getBaseMatrix = getBaseMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
  
  
}



#retrieve/compute inverse of a matrix stored 
#in an instance of makeCacheMatrix
cacheSolve <- function(cachedMatrix, ...){
  
  #get inverse matrix from cache
  inverseMatrix <- cachedMatrix$getInverseMatrix()
  
  #create inverse matrix and cache it if it does not exist
  if(is.null(inverseMatrix)){
    
    #retrieve base matrix
    baseMatrix <- cachedMatrix$getBaseMatrix()
    
    #compute inverse of base matrix
    inverseMatrix <- solve(baseMatrix)
    
    #cache the inverse matrix
    cachedMatrix$setInverseMatrix(inverseMatrix)
    
  }
  
  #return the inverse matrix
  return(inverseMatrix)
  
}


