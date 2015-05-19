## Description of makeCacheMatrix: This function creates a special "matrix".    
## It contains four functions for this purpose. set() is used to make changes 
## to the matrix ang get() can be used to return the matrix that is stored in 
## main function. setsolve() function is used to store the value of the
## input in a variable called inv into the main function makeCacheMatrix() and 
## getsolve() function is used to return this value. 

makeCacheMatrix <- function(x=matrix(nrow=0, ncol=0)) {
        ## inv variable is assigned null value
        inv <- NULL
        ## set() defined here that enables one to change the matrix stored in
        ## the main function and in doing so it resets the inv variable to null.
        set <- function(y) {
                x <<-y
                inv <<- NULL
        }
        ## get() defined below which is used to return the value of the matrix x
        ## stored in the main function.
        get <- function() x
        ## setsolve() stores the value of the input (solve) in the variable:inv
        ## into the main function makeCacheMatrix.
        setsolve <- function(solve) inv <<-solve
        ## getsolve() is used to return the stored value. 
        getsolve <- function() inv
        ## list() stores all the four functions set,get,setsolve and getsolve.
        list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}

## Description of CacheSolve: The input of the second function cacheSolve is 
## the object: (special matrix x). This function calculates the inverse of the
## matirx that was created with makeCacheMatrix function. But first it checks
## if the inverse has already been computed. If so it just returns the inverse
## from the cache and skips the computation. If the inverse does not exist, this
## function goes ahead and computes the inverse and updates the value of the 
## inverse in the cache using setsolve(). 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' using getsolve():
        inv <- x$getsolve()
        ## if loop is used to check the value of inv to see if the inverse
        ## of the matrix exits. If so, it will print the message "getting 
        ## cached inverse and return the original value of inv. The function
        ## ends here.
        if (!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        ## if the inverse of the matrix does not exist, then assign the special
        ## matrix 'x' to data using get(). 
        data <- x$get()
        ## Now the inverse of this data is computed and assigned to the variable
        ## inv.
        inv <-solve(data, ...)
        ## setsolve() is used to set the new inverse of the matrix to the 
        ## variable inv to be stored in 'x'. 
        x$setsolve(inv)
        ## Return the computed inverse.
        inv
}

## The idea behind these functions makeCacheMatrix and cacheSolve is to be able 
## to store the inverse of a matirx that has already been calculated and cache 
## the value of the inverse whenever needed, instead of recomputing it again 
## and again. And if some change has been made to the matirx, then the inverse
## of the new matrix is obtained and stored in the main function makeCacheMatrix. 

# An example to test the functions:

r <- matrix(c(-1, 3, 1, -5), 2,2)
a <- makeCacheMatrix(r)
a$get()
cacheSolve(a)
cacheSolve(a)
# Getting the inverse directly using solve()
solve(r)







