## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
                   m <- NULL                  # sets m default value to NULL (if cacheSolve not used)
                   y <- NULL                  # sets y default value to NULL (if cacheSolve not used )
                   set <-function(y){         # sets value of matrix
                            x <<- y           # caches the input matrix so that cacheSolve check if it has changeed
                            m <<- NULL        # sets the m(matrix inverse if cacheSolve used) value to NULL
                   } 
                   get <-function() x         # get matrix value
                   setinverse <- function(solve) m <<- solve
                   getinverse <- function() m
                   list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)  # create a list to hold the functions     
                   

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                 m <- x$getinverse()        # Test if an inverse has been already calculated
                 if (!is.null(m)){          # Test if cacheSolve has bee performed
                    if (x$setinverse()== x$getinverse()){
                    message ("getting cached data")
                    return (m)		  # Return m in cache
                    }
                  }
                  matrix <- x$get()         # run the get function to get the value of the input matrix
                  x$set(matrix)             # run the set function to cache the value of the matrix
                  m <- solve(matrix,...)    # compute the input matrix inverse value
                  x$setinverse(m)           # run the setinverse function to cache the inverse
                  m                         # return the inverse

}
