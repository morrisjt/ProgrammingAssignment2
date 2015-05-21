## Put comments here that give an overall description of what your
## functions do
## The folling functions are designed to get a square invertible matrix, solve for it's inverse and store
## the inverse in cache memeory to shorten computation time.


## Write a short comment describing this function
## MakeCacheMatrix is a function that calls upon several other function to set and get the square invertible
## matrix.  It also calls upon setMatrix and getInverse to calculate the inverse and store it in cache.
makeCacheMatrix <- function(x = matrix()) 
{
    m<-NULL
    set<-function(y)
    {
        x<<-y
        m<<-NULL
    }
    get<-function()
    {
        return(x)
    }
    setMatrix <-function(z)
    {
        m<<-z
    }
    getInverse<-function()
    {
        return(m)
    }
    list(set=set,get=get,setMatrix=setMatrix,getInverse=getInverse)
}


## Write a short comment describing this function
## cacheSolve is a function that allows the user to implement the function on a matrix that has been set by
## makeCacheMatrix and will disiplay the inverse if it has previously be calculated or will calculate the
## inverse and store it.
cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'
    m<-x$getInverse()
    if(!is.null(m))
    {
        message("Returning cached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setMatrix(m)
    return(m)
}
