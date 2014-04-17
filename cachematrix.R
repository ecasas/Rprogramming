## This function receives a invertible matrix
## and returns a vector which is a 4 elements list
## the matrix, your inverse initially NULL, and 4 functions
# to set o get any of the two matrices
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y) {
    x<<-y
    inv<<-NULL
  }
  get <-function(y) x
  setinverse <- function(inverse) inv <<-inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function receives a makeCacheMatrix
## if the inverse is not cached, calculates it
## and stores this result on makeCacheMatrix and returns the inverse
## if cached prints a message ans returns the inverse
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   inv<-x$getinverse()
   if(!is.null(inv)){
       message("getting cached inverse")
       return(inv)
    }
   invmatrix<-x$get()
   inv<-solve(invmatrix)
   x$setinverse(inv)
   inv
}
