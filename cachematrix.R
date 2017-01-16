# This module implements a cached version of matrix inversion.
# makeCacheMatrix returns a Matrix data structure, 
# which is a list of an index and four functions:
# - setmatrix, which sets a new matrix in the cache.
# - getmatrix, which retrieves the matrix from the cache
# - setinverse, which sets an inverse matrix in the cache
# - getinverse, which gets the inverse matrix from the cache
#
# cacheSolve calculates the inverse of a matrix.
# Its argument is of type Matrix (the list generated bt makeCacheMatrix).
# cacheSolve first checks whether there exists a cached inverse,
# if yes, it returns the cached inverse.
# if not, it retrieves the matrix from the cache, calculates its inverse
# and puts the inverse in the cache,


# makeCacheMatrix stores a matrix and its corresponding inverse matrix
# in a global variable. In order to be usable for more matrices, it
# uses a list of matrices and a list of inverses.
# The first time it is called, it creates the lists, every next time
# it appends the matrices to these list.
# Besides the get and set functions makeCacheMatrix returns also the index.

makeCacheMatrix <- function(x = NULL) {
  if (!exists("CachedMatrix")) {
    CachedMatrix <<- list(x)
    CachedInverse <<- list(NULL)
  } else {
    CachedMatrix <<- c(CachedMatrix, list(x))
    CachedInverse <<- c(CachedInverse, list(NULL))
  }

  
  setmatrix <- function(index, matrix) {
    CachedMatrix[index] <<- list(matrix)
    CachedInverse[index] <<- list(NULL)
  }
  
  getmatrix <- function(index) CachedMatrix[[index]]
  
  setinverse <- function(index, inverse) CachedInverse[index] <<- list(inverse)
  
  getinverse <- function(index) CachedInverse[[index]]
  
  list(index=length(CachedMatrix),
       setmatrix = setmatrix,
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


# cacheSolve first checks whether the inverted matrix is already in cahche.
# If this is the case, the inverted matrix is returned and we are ready.
# If this is not the case, the original matrix is retrieved from the data
# structure in the argument.
# If this original matrix is not set yet, we can't do anything and we quit the function.
# Otherwise, the inverse is calculated, put back in the cache and returned to the caller.

cacheSolve <- function(mtx, ...) {
  inverseMatrix <- mtx$getinverse(mtx$index)
  if(!is.null(inverseMatrix)) {
    message("getting cached inverted matrix")
    return(inverseMatrix)
  }
  
  matrix <- mtx$getmatrix(mtx$index)
  if(is.null(matrix)) {
    message("matrix not set!")
    return(matrix())
  }
    
  message("calculate inverse matrix")
  inverseMatrix <- solve(matrix, ...)
  mtx$setinverse(mtx$index, inverseMatrix)
  inverseMatrix
}


# the below function is meant to test the implemented data structure

smallTest <- function() {
  set.seed(42)
  # generate two matrices
  m1 <- matrix(rnorm(16,1),4,4)
  m2 <- matrix(1:9,3,3)
  m2[2,2]<-1 # otherwise m2 is singular!

  # generate two Matrix data structures
  # the first is initialised by a matrix, the second not
  mat1 <- makeCacheMatrix(m1)
  mat2 <- makeCacheMatrix()
  
  cat("invert matrix m1 in mat1 (4x4), this can not be retrieved from cache yet.\n")
  print(cacheSolve(mat1))

  cat("\ninvert the matrix in mat2, the matrix in mat2 is not set yet. The result should be NA.\n")
  print(cacheSolve(mat2))
  
  cat("\ninvert matrix m1 in mat1 (4x4), this must be retrieved from cache.\n")
  print(cacheSolve(mat1))
  
  cat("\nSet matrix m2 in mat2 (3x3)\n")
  mat2$setmatrix(mat2$index, m2)

  cat("\ninvert matrix m2 (3x3) in mat2, this can not be retrieved from cache yet.\n")
  print(cacheSolve(mat2))
  
  cat("\ninvert matrix m1 (4x4) in mat1, this must be retrieved from cache.\n")
  print(cacheSolve(mat1))
  
  cat("\ninvert matrix m2 (3x3) in mat2, this must be retrieved from cache.\n")
  print(cacheSolve(mat2))

  cat("\nSet matrix m2 (3x3) in mat1.\n")
  mat1$setmatrix(mat1$index, m2)
  
  cat("\ninvert matrix m2 (3x3) in mat1, this can not be retrieved from cache yet.\n")
  print(cacheSolve(mat1))
  
  cat("\ninvert matrix m2 (3x3) in mat2, this must be retrieved from cache.\n")
  print(cacheSolve(mat2))
  
  cat("\ninvert matrix m2 (3x3) in mat1, this must be retrieved from cache.\n")
  print(cacheSolve(mat1))

  cat("\ncreate a third matrix m3 (5x5) in mat3.\n")
  m3 <- matrix(rnorm(25,1),5,5)
  mat3 <- makeCacheMatrix(m3)
  cat("\ninvert matrix m3 (5x5) in mat3, this can not be retrieved from cache yet.\n")
  print(cacheSolve(mat3))
  cat("\ninvert matrix m3 (5x5) in mat3, this must be retrieved from cache.\n")
  print(cacheSolve(mat3))
}
