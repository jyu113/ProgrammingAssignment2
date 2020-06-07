## These are two functions that are used to create a special object that stores a matrix
## and cache's its inverse

## This function can create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set = function(y){
                x <<- y
                inv <<- null
        }
        get = function() x
        setInv = function(inverse) inv <<- inverse
        getInv = function() inv
        list(set = set, get = get, 
             setInv =  setInv, getInv = getInv)
}


## This function fisrt check to see if the inverse has already been cached.
## If so, it get the cached inverse, otherwise, it calculates the inverse of the
## matrix and sets the inverse in the cache via the setInv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getInv()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat.data =  x$get()
        inv = solve(mat.data, ...)
        x$setInv(inv)
        
        return(inv)
        
}
