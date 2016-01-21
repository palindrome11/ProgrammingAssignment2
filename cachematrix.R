## Coursera - R Programming Class by Johns Hopkins University 
## Programming Assignment 2
##
## Program Author and Student:	Christopher Conlon (cwconlon@gmail.com)
##
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## The assignment is to write a pair of functions that cache the inverse 
## of a matrix.
##
## This file has  two functions: 1) makeCacheMatrix() and 2) cacheSolve(). 
## These are codependent functions. They work together and should be sourced
## into your R environment together.
## 
## 
## Once you have sourced these functions into your environment you want to pass
## a square matrix (equal dimensions) into the makeCacheMatrix function and save ## the result to a variable. For example: x <- makeCacheMatrix(a) where
## "a" is the square matrix.
##
## See below for a sample square matrix you can use to test.
#
############
## Quick test data set... 
##
## You can use the below syntax to create a simple square matrix  
## that can be passed into makeCacheMatrix() as the variable "a".
## 
##  a <- matrix(c(4,2,7,6), ncol=2)  
##  print(a)
## 			 [,1] [,2]
## 		[1,]    4    7
## 		[2,]    2    6
##
##  The inverse of the matrix above is obtained with the solve() function. 
##
##  Using the above  matrix in "a", the following is the result.
##
##  solve(a)  
## 			 [,1] [,2]
## 		[1,]  0.6 -0.7
## 		[2,] -0.2  0.4
##
##############
##
## Finally, pass the result returned by your call to the makeCacheMatrix 
## function into the cacheSolve function. This will cache the inverse. 
## Using the example above... this would be cacheSolve(x). 
## 
## If you execute the cacheSolve function again with the same value (x), you 
## will see that a cached result is returned as the function will print this out
## out for you with a "getting cached data" message printed to the screen.
##
##

## The makeMatrix function accepts a square (equal dimensions) and returns a 
## a list of functions that it defines to manipulate that square matrix. In this
## case the following functions are defined by makeMatrix:
##
##	1) set  <<== Take the passed in matrix and add to the "super" environment 
##	where it will be retained and accessible to other functions/methods.
##  2) get  <<== returns the matrix that has been defined 
##  3) setinverse <<== Places the result of the inverse operation in the super
##	environment for access by other functions/methods.
##	4) getinverse <<== returns the result of the setinverse function from the 
##	super environment.

makeMatrix <- function(x = matrix) {
		## the below 2 if statement test for validity of the passed in argument.
        if(!is.matrix(x)) stop("passed argument must be a matrix")
        if(dim(x)[1] != dim(x)[2]) stop("Must be a square matrix to invert")
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function takes a list object returned by the makeMatrix
## function and tests whether it can serve the result from cache, or 
## in other words, the stored result of a prior inverse operation on the same 
## matrix. If there is not an already stored result, then it generates a new 
## new result and stores the new result in the "super" environment using the 
## routines defined in the makeMatrix function.
## 
## Once the result is cached (second and subsequent tried with the same
## matrix, then caceSolve returns the result from the "super" environment.
##

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
