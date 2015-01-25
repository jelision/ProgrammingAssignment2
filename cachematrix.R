## The following two functions cache the inverse of a matrix. Since matrix
##  inversion may involve considerable computational power, specially for big
##  matrices, the following two functions allow to store a matrix and its
##  inverse. If the inverse of the matrix has already been computed, then
##  it will return the saved inverse and will not recomputed it a second
##  time.


## makeCacheMatrix stores within its environment the matrix and its inverse.
##  When called, it returns a list of functions to access and manipulate its
##  content, i.e., the matrix and its inverse:
##      setmatrix()
##      getmatrix()
##      setinverse()
##      getinverse()

makeCacheMatrix <- function(mat = matrix()) {

    ## mat variable stores the the matrix
    ## inv variable stores the inverse of the matrix
    
    inv <- NULL
    
    ## Before storing the new matrix, it checks whether the new matrix is
    ##  actually the same matrix that is already stored. If the matrices are 
    ##  different, then it stores the new matrix and deletes its inverse.
    setmatrix <- function(y)
    {
        if (!identical(mat,y))
        {
          mat <<- y
          inv <<- NULL
        }
        else
        {
          message("new matrix is identical to stored matrix")
        }
    }
  
    getmatrix <- function()
    {
      mat
    }
  
    setinverse <- function(inv_mat)
    {
      inv <<- inv_mat
    }
  
    getinverse <- function()
    {
      inv
    }
  
    list(setmatrix = setmatrix,
         getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse matrix. Its input is the object created by
##  the function makeCacheMatrix and therefore it computes the inverse of the
##  matrix stored within the makeCacheMatrix object. If the inverse has been
##  already computed, then it returns the inverse stored within the
##  makeCacheMatrix object.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of the matrix within 'x'
  
    inv_mat <- x$getinverse()
    
    if(is.null(inv_mat))
    {
      mat <- x$getmatrix()
      
      ## The solve function is called in order to compute the inverse.
      inv_mat <- solve(mat,...)
      
      x$setinverse(inv_mat)
    }
    else
    {
      message("getting cached data")
    }
    
    inv_mat
}
