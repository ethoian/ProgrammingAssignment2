## Put comments here that give an overall description of what your
## functions do
## 1- makeCacheMatrix function creates a special "matrix" object that can cache the input matrix and its inverse
##    Itself calls 4 sub functions inside the "makeCacheMatrix" function:
##    a) "set" function : mainly to cache the inverse matrix value
##      --enter the value of the input matrix object into y with function(y) ex: matrix(data=c(1,0,5,2,1,6,3,4,0), nrow=3,ncol=3)
##      --caches the input matrix object value into a variable x so that the cacheSolve function can check if has changed
##      --assign m variable as the inverse of the matrix, reset it to NULL if cacheSolve used first time
##    b) "get" function: calculate the inverse value and cache it
##      -- load any "x" matrix value into "get" with "function() x"
##      -- calculate the matrix inverse value and put it into "setinverse" function
##      -- get the inverse value with function(solve) on m variable (m <<- solve) and put it into "getinverse" function
##    c) create a list function to load the four functions into a list: "set","get","setinverse","getinverse"
##
## 2- cacheSolve test if the inverse has been calculated and the matrix has not changed
##    a) by testing if "m" is "NULL" we know if cacheSolve has been performed
##    b) by testing if "x$setinverse()=x$getinverse()",we know if the value of the newly entered matrix is identical to the one in cache
##    c) if cacheSolve has been performed and matrix value has not changed just return the calculated inverse "m"
##    d) otherwise calculate the inverse matrix and put it into m
##    e) Return m
## 3- An simple test (non exhaustive):
##    a) testmat1<-matrix(data=c(1,0,5,2,1,6,3,4,0), nrow = 3,ncol= 3)
##    b) testmat2<-matrix(data = c(4,2,7,6), nrow = 2, ncol = 2)
##    c) mat1 <- makeCacheMatrix(testmat1)
##    d) cacheSolve(mat1) this should return the inverse of matrix testmat1
##    e) cacheSolve(mat2) this should return the inverst of matrix testmat2

## Write a short comment describing this function
## 1- The first function, makeCacheMatrix creates a special "matrix" object that can cache the input matrix and its inverse:

makeCacheMatrix <- function(x = matrix()) {
                   m <- NULL                  # sets m default value to NULL (if cacheSolve not used)
                   y <- NULL                  # sets y default value to NULL (if cacheSolve not used )
                   set <-function(y){         # sets value of matrix object value
                            x <<- y           # caches the input matrix so that cacheSolve check if it has changed
                            m <<- NULL        # sets the m(matrix inverse if cacheSolve used) value to NULL
                   } 
                   get <-function() x         # get matrix value
                   setinverse <- function(solve) m <<- solve
                   getinverse <- function() m
                   list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)  # create a list to hold the 4 sub functions     
                   
}


## Write a short comment describing this function
## 2- The second function cacheSolve calls functions stored in the special "matrix" returned by makeCacheMatrix
##    2.1 if the inverse has been calculated and the matrix has not changed then cacheSolve retrieves the inverse from the cache
##    2.2 if it is a new input m the inverse of the new matrix is calculated and the inverse is set in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                 m <- x$getinverse()        # Test if an inverse has been already calculated
                 if (!is.null(m)){          # Test if cacheSolve has been performed
                    message ("getting cached data")
                    return (m)		  # Return m in cache
                 
                  }
                  matrix <- x$get()         # run the get function to get the value of the input matrix
                  x$set(matrix)             # run the set function to cache the value of the matrix
                  m <- solve(matrix,...)    # compute the input matrix inverse value
                  x$setinverse(m)           # run the setinverse function to cache the inverse
                  m                         # return the inverse

 
}
