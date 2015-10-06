## function defines functions for getting and setting input and inverse matrixes
## inv stores cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL #init inverse matrix with NULL
        
        #assign new value to x and reset inverse matrix to NULL
        set<-function(y){
        x<<-y
        inv<<-NULL
        }
        
        get<-function() x # return input matrix
        
        setinverse<-function(sinv) inv<<-sinv #store inverse matrix in variable inv
        getinverse<-function() inv # return inverse matrix
        
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## cacheSolve returns cached version of inverse of matrix if it is available
## oterwise function computes inverse of matrix and store it in a makeCacheMatrix object
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse() # try to get cached inverse matrix
        if(!is.null(inv)) { # if inverse matrix was computed return it as result
                message("getting cached data")
                return(inv)
        }
        data <- x$get() # getting input matrix
        m <- solve(data) #compute inverse matrix
        x$setinverse(m) # store inverse matrix in makeCacheMatrix
        m #return inverse matrix result
}
