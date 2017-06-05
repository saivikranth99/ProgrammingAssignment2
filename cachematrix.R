###################cachematrix.R-------> sai vikranth##################################

## Put comments here that give an overall description of what your
## functions do

##These functions are written as part of assignment on R programming course by coursera


## Write a short comment describing this function


##This function creates a special matrix object that can cache its inverse
##This function takes matrix as an input
makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        
        set<-function(y)          ##set value of matrix
                {
                x<<-y
                inv<<-NULL
                }
        get<-function() x          ##get value of the matrix
        
        setInverse<-function(inverse) inv<<-inverse          ##set value of invertible matrix
        
        getInverse<-function() inv                           ##get value of invertible matrix
        
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

 
        

}


## Write a short comment describing this function

-## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
-# input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
-# In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
-# and set the invertible  matrix by using the solve function.
-# In case inverse matrix from makeCacheMatrix((matrix) has some value in it (always works
-#after running the code 1st time), it returns a message  "Getting Cached Invertible Matrix" 
-#and the cached object
-  
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       inv<-x$getInverse()  
        
        if(!is.null(inv)) {                ##if inverse matrix is not null
                
                 message("Getting Cached Invertible Matrix")   #Type message: Getting Cached Invertible Matrix 
                
-          return(inv)                             #return the invertible matrix
-        }
-          
-#if value of the invertible matrix is NULL then  
-        MatrixData <- x$get()                     #get the original Matrix Data 
        
-        inv <- solve(MatrixData, ...)             #use solve function to inverse the matrix
        
-        x$setInverse(inv)                         #set the invertible matrix 
        
-        return(inv)                               #return the invertible matrix
        
       ## Return a matrix that is the inverse of 'x'
 }
}
