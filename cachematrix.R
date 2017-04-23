## Creates an object matrix whose inverse gets cached
## when calculated for the first time, so it doesn't have
## to be recalculated in later calls

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x<<-y
        inv<<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv<<-solve
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    inv<-x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data, ...)
    x$setinv(inv)
    inv
}
