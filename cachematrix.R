## The inverse of a square matrix can be computed with the "solve" function
## in R. If X is a square invertible matrix, then solve(X) returns its inverse.
##
## Calculating the inverse of a matrix can cost quite some computational power.
## Below you will find two functions that, once used together in the right way,
## will make sure that the result of the function, in this case the inverse
## of the matrix will be "saved" for future use, so you don't have to ask the
## computer to re-do the calculation when you need it again.
## 
## It uses the scoping rules, specifically lexical scoping, of R that create
## a unique environment for every function you run in R. In this case a sort
## of sub-environment will be created in which the result of the "solve()"
## function will be stored.
##
## The first function "makeCacheMatrix" creates a special "matrix" object
## that can cache its inverse. So in this object the inverse of the matrix
## will be "saved". The key to this trick is in the "<<-" operator, which
## causes a search through parent environments for an existing definition
## of the variable being assigned. If such a variable is found (and its
## binding is not locked) then its value is redefined, otherwise assignment
## takes place in the global environment.
##
## Before you can use this function, define a matrix called x. This matrix
## should be a square and it should be invertable.
## For example: x <- matrix(1:4,2,2) to make a matrix of 2 by 2 with the
## number 1 to 4 devided over the columns and rows.
## Now use the first function below by defining an object (e.g. "CM") that
## is the makeCacheMatrix of x. This is done by stating:
## CM <- makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## The second function either retrieves the "saved" result (after the
## second time you run it), or, when that is not available (the first
## time you run it), it will calculate the inverse of the matrix and "save" it.
##
## In other words:
## cacheSolve: computes the inverse of the special "matrix" returned by
##             makeCacheMatrix above. If the inverse has already been
##             calculated (and the matrix has not changed), then
##             cacheSolve retrieves the inverse from the cache.
##
## You use this function after the first and you call it on the object you
## just created: cacheSolve(CM)


cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
            ## above this is the check to see whether there is a result in cache
            ## below this point is the part that calculates the inverse if that
            ## result is not available from cache
    
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
  }      
  
}

## The inverse of the matrix will be printed. To check this result, you can 
## define an object for the inverse matrix (I) by using: I <- cacheSolve(CM)
## and then multiply the matrix with its inverse by using: x %*% I 
## this should return the Identity matrix.
##
## If you run cacheSolve(CM) again, you will see the message "getting cached
## data" to indicate it has used the first part of its body and it retrieved
## the result from the cache, the "saved" result.
##
## To change the input matrix, use e.g: CM$set(matrix(c(4,16,4,8),nrow=2,ncol=2))
## and then you only need to run the second function again. Not also the first,
## as you don't have to create a new environment again.
##
##
## For comment above I've used "R Documentation" in RStudio, the course forum
## and the external links mentioned in the forum.




