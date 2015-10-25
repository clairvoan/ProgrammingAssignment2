## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
mainv <- NULL
scea <- function(y){
  x <<- y
  mainv <<- NULL
}
sceb <- function() x
sceainv <- function(invcal) mainv <<- invcal
scebinv <- function() mainv
list(scea = scea, 
     sceb = sceb, 
     sceainv = sceainv,
     scebinv = scebinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
mainv <- x$scebinv()
  if (!is.null(mainv)) {
    message("data catching")
    return (mainv)
  }
   temp <- x$sceb()
   mainv <- solve(temp, ...)
   x$sceainv(mainv)
   mainv
}
