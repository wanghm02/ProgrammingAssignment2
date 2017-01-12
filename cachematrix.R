## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv<-NULL
        set<-function(y) {
                x<<-y
                inv<<-NULL
        }
        
        get<-function() x
        setinverse<-function (invMat) inv<<-invMat
        getinverse<-function () inv
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.

## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
        if(!is.null(inv)) {
                message("getting cache data")
                return (inv)
        }
        data<-x$get()
        inv<-solve(data)
        x$setinverse(inv)
        inv
}

## Sample run:
## give matrix x value
## > x<-matrix(c(2,0,0,2),c(2,2))
## > x
## [,1] [,2]
## [1,]    2    0
## [2,]    0    2

## > m=makeCacheMatrix(x)
## get m
## > m$get()
## [,1] [,2]
## [1,]    2    0
## [2,]    0    2

## No Cache in the first run
## > cacheSolve(m)
## [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5

## Retrieve from the cache in the second run
## > cacheSolve(m)
## getting cache data
## [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5
