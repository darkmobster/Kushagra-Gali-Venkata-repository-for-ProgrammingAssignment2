## Put comments here that give an overall description of what your
## functions do

# Given that calculating the inverse of a matrix is expensive, it is beneficial to cache the inverse in memory through the use of the cachemean() funcrion instead of repeatedly calculating the inverse.
# The role of the two functions, makeVector() and cachemean() is essentially to cache the inverse of a matrix.
#The first function's purpose is to create an R object that stores a matrix and its mean. 
#The second function needs the first function to work because in order to reterieve the mean from the cached value that is stored in the first function's parent environment, cachmean() requires an argument retruned by makeVector().

## Write a short comment describing this function
#makeCacheMatrix creates a list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {

inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation.
#The function requires an input argument of from makeVector() because otherwise any other nomral vector will yield an error message. 
#If not, it computes the inverse, sets the value in the cache via setinverse function.
# This again explains how the makeVector() function is incomplete without the cachemean() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

## Sample run to check my code:

 x = rbind(c(1, -1/8), c(-1/8, 1))
 m = makeCacheMatrix(x)
 m$get()
      [,1]   [,2]
[1,]  1.000 -0.125
[2,] -0.125  1.000


 No cache in the first run
 cacheSolve(m)
      [,1]      [,2]
[1,] 1.0158730 0.1269841
[2,] 0.1269841 1.0158730


##Retrieving from the cache in the second run
 cacheSolve(m)
## getting cached data.
      [,1]      [,2]
[1,] 1.0158730 0.1269841
[2,] 0.1269841 1.0158730
>


