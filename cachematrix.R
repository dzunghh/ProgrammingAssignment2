#### Data Science Specialization Course 2 - R Programming - Week 3 Assignment 2


## These functions are to cache the inverse of a matrix using makeCacheMatrix 
## and cacheSolve without calculating it more than once

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        #set initial value to Null
        i <- NULL
        
        #set() function enables the user to enter a valid matrix. When user enters
        # new matrix, then x needs to be changed to the passed in matrix and 
        # i needs to be reset to NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        #get() function helps to return the stored matrix
        get <- function() x
        
        #setcache() function is used by the cachesolve() function to cache the 
        #value of the inverse matrix (this modifies the value i)
        setcache <- function(inverse) i <<- inverse
        
        #getcache() function is used by the cachesolve() function to retrieve 
        #the value of the cached inverse matrix
        getcache <- function() i
        
        #return the list with these functions created this list is the "cache" matrix
        list (set = set, get = get,
              setcache = setcache, getcache = getcache)
}


## This function computes the inverse of the special "matrix" returned 
# by `makeCacheMatrix` above. If the inverse has already been calculated 
# (and the matrix has not changed), then then it
# is simply looked up and returned. Otherwise, the inverse
# is calculated and stored in the cacheMatrix and then returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #  Inverse of the matrix stored using makeCacheMatrix() is calculated 
        i <- x$getcache()
        
        #condition to check whether the original matrix has been modified
        if(!is.null(i)) {
                message("getting cached data")
                #CONDITION IS TRUE: the cached value of the inverse matrix is returned
                return(i)
        }
        
        #CONDITION IS FALSE: inverse of the given matrix is calulated anew
        data <- x$get()
        i <- solve(data, ...)
        x$setcache(i)
        
        #the inverse matrix is returned
        i
}


### Test the functions

# Create an object to store the matrix and cache its inverse
cacheableMatrix = makeCacheMatrix(rbind(c(20, 30), c(40, 50)) )

# The first call to cacheSolve will calculate the inverse,
inverse1 = cacheSolve(cacheableMatrix)

# Further calls to cacheSolve will be significantly faster, since
# they return the value cached during the previous call.
inverse2 = cacheSolve(cacheableMatrix)



