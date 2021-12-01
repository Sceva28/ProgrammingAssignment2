## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## library mass is used to calculate inverse for squared and non squared matrices 
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL         ##initializing inverse as null
  set<- function(y){
                    x<<-y
                    inv<<-NULL
  }
  get<-function()x ## function to get matrix x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
        inver<-ginv(x) 
        inver%*%x  ## function to obtain inverse of the matrix
  }
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}

    ##function for capturing cache data

cachesolve <- function(x, ...)  ## gets cache data 
    {
  inv<-x$getinv()
  if(!is.null(inv)) ##checking whether inverse is null
    
    {
    message("getting cached data!!")
    return(inv) ## returns inverse value
  }
  data <- x$get()
  inv<-solve(data...) ## calculates inverse value
  x$setinv(inv)
  inv ## returns a matrix which is the inverse of 'x'
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
