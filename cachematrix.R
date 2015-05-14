## The first function makes a list with four functions, to set a matrix, get a
## matrix, set the inverse and get the inverse. The second function takes as
## argument the list made by the first function, and either calculates the
## inverse of the matrix, or reads the inverse from the cache, if calculated
## before.


## This function takes as input a matrix. It outputs a list containing 4
## functions.
## 1. Set - this sets the matrix. This is not required to solve the exercise,
##      but is nice to have.
## 2. Get - this gets the matrix.
## 3. Setinverse - this stores a number to the cache, which should be the
##      inverse of the matrix (calculated in the next function)
## 4. Getinverse - this gets the number stored in the cache. If setinverse has
##      not been called, the number will be NULL.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




## This function takes as input the list made from the previous function. 
## First it calls the getinverse function from the list. If getinverse returns
## a number (or not NULL), it will let you know that the cache is used, and the 
## number returned. If the getinverse function returns NULL, then the inverse of 
## the matrix will be calculated, and sent to "storage"/cache using the 
## setinverse function from above. The calculated inverse will be returned.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
